/****************************************************************************
**
** Copyright (C) 2013 Digia Plc and/or its subsidiary(-ies).
** Contact: http://www.qt-project.org/legal
**
** This file is part of the examples of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Digia.  For licensing terms and
** conditions see http://qt.digia.com/licensing.  For further information
** use the contact form at http://qt.digia.com/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Digia gives you certain additional
** rights.  These rights are described in the Digia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
**
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "fileinfothread_p.h"
#include <qdiriterator.h>

#include <QDebug>


FileInfoThread::FileInfoThread(QObject *parent)
    : QThread(parent),
      abort(false),
#ifndef QT_NO_FILESYSTEMWATCHER
      watcher(0),
#endif
      sortFlags(QDir::Name),
      needUpdate(true),
      folderUpdate(false),
      sortUpdate(false),
      showDirs(true),
      showDirsFirst(false),
      showDotDot(false),
      showOnlyReadable(false)
{
#ifndef QT_NO_FILESYSTEMWATCHER
    watcher = new QFileSystemWatcher(this);
    connect(watcher, SIGNAL(directoryChanged(QString)), this, SLOT(dirChanged(QString)));
    connect(watcher, SIGNAL(fileChanged(QString)), this, SLOT(updateFile(QString)));
#endif // !QT_NO_FILESYSTEMWATCHER
    start(LowPriority);
}

FileInfoThread::~FileInfoThread()
{
    QMutexLocker locker(&mutex);
    abort = true;
    condition.wakeOne();
    locker.unlock();
    wait();
}

void FileInfoThread::clear()
{
    QMutexLocker locker(&mutex);
#ifndef QT_NO_FILESYSTEMWATCHER
    watcher->removePaths(watcher->files());
    watcher->removePaths(watcher->directories());
#endif
}

void FileInfoThread::removePath(const QString &path)
{
    QMutexLocker locker(&mutex);
#ifndef QT_NO_FILESYSTEMWATCHER
    watcher->removePath(path);
#endif
    currentPath.clear();
}

void FileInfoThread::setPath(const QString &path)
{
    Q_ASSERT(!path.isEmpty());

    QMutexLocker locker(&mutex);
#ifndef QT_NO_FILESYSTEMWATCHER
    watcher->addPath(path);
#endif
    currentPath = path;
    needUpdate = true;
    condition.wakeAll();
}

void FileInfoThread::setRootPath(const QString &path)
{
    Q_ASSERT(!path.isEmpty());

    QMutexLocker locker(&mutex);
    rootPath = path;
}

#ifndef QT_NO_FILESYSTEMWATCHER
void FileInfoThread::dirChanged(const QString &directoryPath)
{
    Q_UNUSED(directoryPath);
    QMutexLocker locker(&mutex);
    folderUpdate = true;
    condition.wakeAll();
}
#endif

void FileInfoThread::setSortFlags(QDir::SortFlags flags)
{
    QMutexLocker locker(&mutex);
    sortFlags = flags;
    sortUpdate = true;
    condition.wakeAll();
}

void FileInfoThread::setNameFilters(const QStringList & filters)
{
    QMutexLocker locker(&mutex);
    nameFilters = filters;
    folderUpdate = true;
    condition.wakeAll();
}

void FileInfoThread::setShowDirs(bool showFolders)
{
    QMutexLocker locker(&mutex);
    showDirs = showFolders;
    folderUpdate = true;
    condition.wakeAll();
}

void FileInfoThread::setShowDirsFirst(bool show)
{
    QMutexLocker locker(&mutex);
    showDirsFirst = show;
    folderUpdate = true;
    condition.wakeAll();
}

void FileInfoThread::setShowDotDot(bool on)
{
    QMutexLocker locker(&mutex);
    showDotDot = on;
    folderUpdate = true;
    condition.wakeAll();
}

void FileInfoThread::setShowOnlyReadable(bool on)
{
    QMutexLocker locker(&mutex);
    showOnlyReadable = on;
    folderUpdate = true;
    condition.wakeAll();
}

#ifndef QT_NO_FILESYSTEMWATCHER
void FileInfoThread::updateFile(const QString &path)
{
    Q_UNUSED(path);
    QMutexLocker locker(&mutex);
    folderUpdate = true;
    condition.wakeAll();
}
#endif

void FileInfoThread::run()
{
    forever {
        bool updateFiles = false;
        QMutexLocker locker(&mutex);
        if (abort) {
            return;
        }
        if (currentPath.isEmpty() || !needUpdate)
            condition.wait(&mutex);

        if (abort) {
            return;
        }

        if (!currentPath.isEmpty()) {
            updateFiles = true;
        }
        if (updateFiles)
            getFileInfos(currentPath);
        locker.unlock();
    }
}


void FileInfoThread::getFileInfos(const QString &path)
{
    QDir::Filters filter;
    filter = QDir::Files | QDir::NoDot | QDir::CaseSensitive;
    if (showDirs)
        filter = filter | QDir::AllDirs | QDir::Drives;
    if ((path == rootPath) || !showDotDot)
        filter = filter | QDir::NoDotDot;
    if (showOnlyReadable)
        filter = filter | QDir::Readable;
    if (showDirsFirst)
        sortFlags = sortFlags | QDir::DirsFirst;

    QDir currentDir(path, QString(), sortFlags);
    QFileInfoList fileInfoList;
    QList<FileProperty> filePropertyList;

    fileInfoList = currentDir.entryInfoList(nameFilters, filter, sortFlags);

    if (!fileInfoList.isEmpty()) {
        foreach (QFileInfo info, fileInfoList) {
            //qDebug() << "Adding file : " << info.fileName() << "to list ";
            filePropertyList << FileProperty(info);
        }
        if (folderUpdate) {
            int fromIndex = 0;
            int toIndex = currentFileList.size()-1;
            findChangeRange(filePropertyList, fromIndex, toIndex);
            folderUpdate = false;
            currentFileList = filePropertyList;
            //qDebug() << "emit directoryUpdated : " << fromIndex << " " << toIndex;
            emit directoryUpdated(path, filePropertyList, fromIndex, toIndex);
        } else {
            currentFileList = filePropertyList;
            if (sortUpdate) {
                emit sortFinished(filePropertyList);
                sortUpdate = false;
            } else
                emit directoryChanged(path, filePropertyList);
        }
    } else {
        // The directory is empty
        if (folderUpdate) {
            int fromIndex = 0;
            int toIndex = currentFileList.size()-1;
            folderUpdate = false;
            currentFileList.clear();
            emit directoryUpdated(path, filePropertyList, fromIndex, toIndex);
        } else {
            currentFileList.clear();
            emit directoryChanged(path, filePropertyList);
        }
    }
    needUpdate = false;
}

void FileInfoThread::findChangeRange(const QList<FileProperty> &list, int &fromIndex, int &toIndex)
{
    if (currentFileList.size() == 0) {
        fromIndex = 0;
        toIndex = list.size();
        return;
    }

    int i;
    int listSize = list.size() < currentFileList.size() ? list.size() : currentFileList.size();
    bool changeFound = false;

    for (i=0; i < listSize; i++) {
        if (list.at(i) != currentFileList.at(i)) {
            changeFound = true;
            break;
        }
    }

    if (changeFound)
        fromIndex = i;
    else
        fromIndex = i-1;

    // For now I let the rest of the list be updated..
    toIndex = list.size() > currentFileList.size() ? list.size() - 1 : currentFileList.size() - 1;
}
