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

//![code]
#include "qquickfolderlistmodel.h"
#include "fileinfothread_p.h"
#include "fileproperty_p.h"
#include <qqmlcontext.h>
#include <qqmlfile.h>

QT_BEGIN_NAMESPACE

class QQuickFolderListModelPrivate
{
    Q_DECLARE_PUBLIC(QQuickFolderListModel)

public:
    QQuickFolderListModelPrivate(QQuickFolderListModel *q)
        : q_ptr(q),
          sortField(QQuickFolderListModel::Name), sortReversed(false), showDirs(true), showDirsFirst(false), showDots(false), showOnlyReadable(false)
    {
        nameFilters << QLatin1String("*");
    }


    QQuickFolderListModel *q_ptr;
    QUrl currentDir;
    QUrl rootDir;
    FileInfoThread fileInfoThread;
    QList<FileProperty> data;
    QHash<int, QByteArray> roleNames;
    QQuickFolderListModel::SortField sortField;
    QStringList nameFilters;
    bool sortReversed;
    bool showDirs;
    bool showDirsFirst;
    bool showDots;
    bool showOnlyReadable;

    ~QQuickFolderListModelPrivate() {}
    void init();
    void updateSorting();

    // private slots
    void _q_directoryChanged(const QString &directory, const QList<FileProperty> &list);
    void _q_directoryUpdated(const QString &directory, const QList<FileProperty> &list, int fromIndex, int toIndex);
    void _q_sortFinished(const QList<FileProperty> &list);

    static QString resolvePath(const QUrl &path);
};


void QQuickFolderListModelPrivate::init()
{
    Q_Q(QQuickFolderListModel);
    qRegisterMetaType<QList<FileProperty> >("QList<FileProperty>");
    q->connect(&fileInfoThread, SIGNAL(directoryChanged(QString, QList<FileProperty>)),
               q, SLOT(_q_directoryChanged(QString, QList<FileProperty>)));
    q->connect(&fileInfoThread, SIGNAL(directoryUpdated(QString, QList<FileProperty>, int, int)),
               q, SLOT(_q_directoryUpdated(QString, QList<FileProperty>, int, int)));
    q->connect(&fileInfoThread, SIGNAL(sortFinished(QList<FileProperty>)),
               q, SLOT(_q_sortFinished(QList<FileProperty>)));
}


void QQuickFolderListModelPrivate::updateSorting()
{
    Q_Q(QQuickFolderListModel);

    QDir::SortFlags flags = 0;

    switch (sortField) {
        case QQuickFolderListModel::Unsorted:
            flags |= QDir::Unsorted;
            break;
        case QQuickFolderListModel::Name:
            flags |= QDir::Name;
            break;
        case QQuickFolderListModel::Time:
            flags |= QDir::Time;
            break;
        case QQuickFolderListModel::Size:
            flags |= QDir::Size;
            break;
        case QQuickFolderListModel::Type:
            flags |= QDir::Type;
            break;
        default:
            break;
    }

    emit q->layoutAboutToBeChanged();

    if (sortReversed)
        flags |= QDir::Reversed;

    fileInfoThread.setSortFlags(flags);
}

void QQuickFolderListModelPrivate::_q_directoryChanged(const QString &directory, const QList<FileProperty> &list)
{
    Q_Q(QQuickFolderListModel);
    Q_UNUSED(directory);

    data = list;
    q->endResetModel();
    emit q->rowCountChanged();
    emit q->folderChanged();
}


void QQuickFolderListModelPrivate::_q_directoryUpdated(const QString &directory, const QList<FileProperty> &list, int fromIndex, int toIndex)
{
    Q_Q(QQuickFolderListModel);
    Q_UNUSED(directory);

    QModelIndex parent;
    if (data.size() > list.size()) {
        //File(s) removed. Since I do not know how many
        //or where I need to update the whole list from the first item.
        data = list;
        q->beginRemoveRows(parent, fromIndex, toIndex);
        q->endRemoveRows();
        if (list.size() > 0) {
            q->beginInsertRows(parent, fromIndex, list.size()-1);
            q->endInsertRows();
        }
        emit q->rowCountChanged();
    } else if (data.size() < list.size()) {
        //qDebug() << "File added. FromIndex: " << fromIndex << " toIndex: " << toIndex << " list size: " << list.size();
        //File(s) added. Calculate how many and insert
        //from the first changed one.
        toIndex = fromIndex + (list.size() - data.size()-1);
        q->beginInsertRows(parent, fromIndex, toIndex);
        q->endInsertRows();
        data = list;
        emit q->rowCountChanged();
        QModelIndex modelIndexFrom = q->createIndex(fromIndex, 0);
        QModelIndex modelIndexTo = q->createIndex(toIndex, 0);
        emit q->dataChanged(modelIndexFrom, modelIndexTo);
    } else {
        //qDebug() << "File has been updated";
        QModelIndex modelIndexFrom = q->createIndex(fromIndex, 0);
        QModelIndex modelIndexTo = q->createIndex(toIndex, 0);
        data = list;
        emit q->dataChanged(modelIndexFrom, modelIndexTo);
    }
}

void QQuickFolderListModelPrivate::_q_sortFinished(const QList<FileProperty> &list)
{
    Q_Q(QQuickFolderListModel);

    QModelIndex parent;
    q->beginRemoveRows(parent, 0, data.size()-1);
    data.clear();
    q->endRemoveRows();

    q->beginInsertRows(parent, 0, list.size()-1);
    data = list;
    q->endInsertRows();
}

QString QQuickFolderListModelPrivate::resolvePath(const QUrl &path)
{
    QString localPath = QQmlFile::urlToLocalFileOrQrc(path);
    QUrl localUrl = QUrl(localPath);
    QString fullPath = localUrl.path();
    if (localUrl.scheme().length())
      fullPath = localUrl.scheme() + ":" + fullPath;
    return QDir::cleanPath(fullPath);
}

/*!
    \qmlmodule Qt.labs.folderlistmodel 1.0
    \title Qt Labs FolderListModel QML Types
    \ingroup qmlmodules
    \brief The FolderListModel provides a model of the contents of a file system folder.

    To use this module, import the module with the following line:

    \code
    import Qt.labs.folderlistmodel 1.0
    \endcode
*/


/*!
    \qmltype FolderListModel
    \inqmlmodule Qt.labs.folderlistmodel 1.0
    \instantiates QQuickFolderListModel
    \ingroup qtquick-models
    \brief The FolderListModel provides a model of the contents of a file system folder.

    FolderListModel provides access to information about the contents of a folder
    in the local file system, exposing a list of files to views and other data components.

    \note This type is made available by importing the \c Qt.labs.folderlistmodel module.
    \e{Elements in the Qt.labs module are not guaranteed to remain compatible
    in future versions.}

    \b{import Qt.labs.folderlistmodel 1.0}

    The \l folder property specifies the folder to access. Information about the
    files and directories in the folder is supplied via the model's interface.
    Components access names and paths via the following roles:

    \list
    \li \c fileName
    \li \c filePath
    \li \c fileBaseName
    \li \c fileSuffix
    \li \c fileSize
    \li \c fileModified
    \li \c fileAccessed
    \li \c fileIsDir
    \endlist

    Additionally a file entry can be differentiated from a folder entry via the
    isFolder() method.

    \section1 Filtering

    Various properties can be set to filter the number of files and directories
    exposed by the model.

    The \l nameFilters property can be set to contain a list of wildcard filters
    that are applied to names of files and directories, causing only those that
    match the filters to be exposed.

    Directories can be included or excluded using the \l showDirs property, and
    navigation directories can also be excluded by setting the \l showDotAndDotDot
    property to false.

    It is sometimes useful to limit the files and directories exposed to those
    that the user can access. The \l showOnlyReadable property can be set to
    enable this feature.

    \section1 Example Usage

    The following example shows a FolderListModel being used to provide a list
    of QML files in a \l ListView:

    \snippet qml/folderlistmodel.qml 0

    \section1 Path Separators

    Qt uses "/" as a universal directory separator in the same way that "/" is
    used as a path separator in URLs. If you always use "/" as a directory
    separator, Qt will translate your paths to conform to the underlying
    operating system.

    \sa {QML Data Models}
*/

QQuickFolderListModel::QQuickFolderListModel(QObject *parent)
    : QAbstractListModel(parent), d_ptr(new QQuickFolderListModelPrivate(this))
{
    Q_D(QQuickFolderListModel);
    d->roleNames[FileNameRole] = "fileName";
    d->roleNames[FilePathRole] = "filePath";
    d->roleNames[FileBaseNameRole] = "fileBaseName";
    d->roleNames[FileSuffixRole] = "fileSuffix";
    d->roleNames[FileSizeRole] = "fileSize";
    d->roleNames[FileLastModifiedRole] = "fileModified";
    d->roleNames[FileLastReadRole] = "fileAccessed";
    d->roleNames[FileIsDirRole] = "fileIsDir";
    d->init();
}

QQuickFolderListModel::~QQuickFolderListModel()
{
}

QVariant QQuickFolderListModel::data(const QModelIndex &index, int role) const
{
    Q_D(const QQuickFolderListModel);
    QVariant rv;

    if (index.row() >= d->data.size())
        return rv;

    switch (role)
    {
        case FileNameRole:
            rv = d->data.at(index.row()).fileName();
            break;
        case FilePathRole:
            rv = d->data.at(index.row()).filePath();
            break;
        case FileBaseNameRole:
            rv = d->data.at(index.row()).baseName();
            break;
        case FileSuffixRole:
            rv = d->data.at(index.row()).suffix();
            break;
        case FileSizeRole:
            rv = d->data.at(index.row()).size();
            break;
        case FileLastModifiedRole:
            rv = d->data.at(index.row()).lastModified();
            break;
        case FileLastReadRole:
            rv = d->data.at(index.row()).lastRead();
            break;
        case FileIsDirRole:
            rv = d->data.at(index.row()).isDir();
            break;
        default:
            break;
    }
    return rv;
}

QHash<int, QByteArray> QQuickFolderListModel::roleNames() const
{
    Q_D(const QQuickFolderListModel);
    return d->roleNames;
}

/*!
    \qmlproperty int FolderListModel::count

    Returns the number of items in the current folder that match the
    filter criteria.
*/
int QQuickFolderListModel::rowCount(const QModelIndex &parent) const
{
    Q_D(const QQuickFolderListModel);
    Q_UNUSED(parent);
    return d->data.size();
}

QModelIndex QQuickFolderListModel::index(int row, int , const QModelIndex &) const
{
    return createIndex(row, 0);
}

/*!
    \qmlproperty string FolderListModel::folder

    The \a folder property holds a URL for the folder that the model is
    currently providing.

    The value is a URL expressed as a string, and must be a \c file: or \c qrc:
    URL, or a relative URL.

    By default, the value is an invalid URL.
*/
QUrl QQuickFolderListModel::folder() const
{
    Q_D(const QQuickFolderListModel);
    return d->currentDir;
}

void QQuickFolderListModel::setFolder(const QUrl &folder)
{
    Q_D(QQuickFolderListModel);

    if (folder == d->currentDir)
        return;

    QString resolvedPath = QQuickFolderListModelPrivate::resolvePath(folder);

    beginResetModel();

    //Remove the old path for the file system watcher
    if (!d->currentDir.isEmpty())
        d->fileInfoThread.removePath(d->currentDir.path());

    d->currentDir = folder;

    QFileInfo info(resolvedPath);
    if (!info.exists() || !info.isDir()) {
        d->data.clear();
        endResetModel();
        emit rowCountChanged();
        return;
    }

    d->fileInfoThread.setPath(resolvedPath);
}


/*!
   \qmlproperty string QQuickFolderListModel::rootFolder

   When the rootFolder is set, then this folder will
   be threated as the root in the file system, so that
   you can only travers sub folders from this rootFolder.
*/
QUrl QQuickFolderListModel::rootFolder() const
{
    Q_D(const QQuickFolderListModel);
    return d->rootDir;
}

void QQuickFolderListModel::setRootFolder(const QUrl &path)
{
    Q_D(QQuickFolderListModel);

    if (path.isEmpty())
        return;

    QString resolvedPath = QQuickFolderListModelPrivate::resolvePath(path);

    QFileInfo info(resolvedPath);
    if (!info.exists() || !info.isDir())
        return;

    d->fileInfoThread.setRootPath(resolvedPath);
    d->rootDir = path;
}


/*!
    \qmlproperty url FolderListModel::parentFolder

    Returns the URL of the parent of of the current \l folder.
*/
QUrl QQuickFolderListModel::parentFolder() const
{
    Q_D(const QQuickFolderListModel);

    QString localFile = d->currentDir.toLocalFile();
    if (!localFile.isEmpty()) {
        QDir dir(localFile);
#if defined(Q_OS_WIN)
        if (dir.isRoot())
            dir.setPath("");
        else
#endif
            dir.cdUp();
        localFile = dir.path();
    } else {
        int pos = d->currentDir.path().lastIndexOf(QLatin1Char('/'));
        if (pos == -1)
            return QUrl();
        localFile = d->currentDir.path().left(pos);
    }
    return QUrl::fromLocalFile(localFile);
}

/*!
    \qmlproperty list<string> FolderListModel::nameFilters

    The \a nameFilters property contains a list of file name filters.
    The filters may include the ? and * wildcards.

    The example below filters on PNG and JPEG files:

    \qml
    FolderListModel {
        nameFilters: [ "*.png", "*.jpg" ]
    }
    \endqml

    \note Directories are not excluded by filters.
*/
QStringList QQuickFolderListModel::nameFilters() const
{
    Q_D(const QQuickFolderListModel);
    return d->nameFilters;
}

void QQuickFolderListModel::setNameFilters(const QStringList &filters)
{
    Q_D(QQuickFolderListModel);
    d->fileInfoThread.setNameFilters(filters);
    d->nameFilters = filters;
}

void QQuickFolderListModel::classBegin()
{
}

void QQuickFolderListModel::componentComplete()
{
    Q_D(QQuickFolderListModel);

    if (!d->currentDir.isValid() || d->currentDir.toLocalFile().isEmpty() || !QDir().exists(d->currentDir.toLocalFile()))
        setFolder(QUrl(QLatin1String("file://")+QDir::currentPath()));
}

/*!
    \qmlproperty enumeration FolderListModel::sortField

    The \a sortField property contains field to use for sorting.  sortField
    may be one of:
    \list
    \li Unsorted - no sorting is applied.
    \li Name - sort by filename
    \li LastModified - sort by time modified
    \li Size - sort by file size
    \li Type - sort by file type (extension)
    \endlist

    \sa sortReversed
*/
QQuickFolderListModel::SortField QQuickFolderListModel::sortField() const
{
    Q_D(const QQuickFolderListModel);
    return d->sortField;
}

void QQuickFolderListModel::setSortField(SortField field)
{
    Q_D(QQuickFolderListModel);
    if (field != d->sortField) {
        d->sortField = field;
        d->updateSorting();
    }
}

int QQuickFolderListModel::roleFromString(const QString &roleName) const
{
    Q_D(const QQuickFolderListModel);
    return d->roleNames.key(roleName.toLatin1(), -1);
}

/*!
    \qmlproperty bool FolderListModel::sortReversed

    If set to true, reverses the sort order.  The default is false.

    \sa sortField
*/
bool QQuickFolderListModel::sortReversed() const
{
    Q_D(const QQuickFolderListModel);
    return d->sortReversed;
}

void QQuickFolderListModel::setSortReversed(bool rev)
{
    Q_D(QQuickFolderListModel);

    if (rev != d->sortReversed) {
        d->sortReversed = rev;
        d->updateSorting();
    }
}

/*!
    \qmlmethod bool FolderListModel::isFolder(int index)

    Returns true if the entry \a index is a folder; otherwise
    returns false.
*/
bool QQuickFolderListModel::isFolder(int index) const
{
    if (index != -1) {
        QModelIndex idx = createIndex(index, 0);
        if (idx.isValid()) {
            QVariant var = data(idx, FileIsDirRole);
            if (var.isValid())
                return var.toBool();
        }
    }
    return false;
}

/*!
    \qmlproperty bool FolderListModel::showDirs

    If true, directories are included in the model; otherwise only files
    are included.

    By default, this property is true.

    Note that the nameFilters are not applied to directories.

    \sa showDotAndDotDot
*/
bool QQuickFolderListModel::showDirs() const
{
    Q_D(const QQuickFolderListModel);
    return d->showDirs;
}

void  QQuickFolderListModel::setShowDirs(bool on)
{
    Q_D(QQuickFolderListModel);

    d->fileInfoThread.setShowDirs(on);
    d->showDirs = on;
}

/*!
    \qmlproperty bool FolderListModel::showDirsFirst

    If true, if directories are included in the model they will
    always be shown first, then the files.

    By default, this property is false.

*/
bool QQuickFolderListModel::showDirsFirst() const
{
    Q_D(const QQuickFolderListModel);
    return d->showDirsFirst;
}

void  QQuickFolderListModel::setShowDirsFirst(bool on)
{
    Q_D(QQuickFolderListModel);

    d->fileInfoThread.setShowDirsFirst(on);
    d->showDirsFirst = on;
}


/*!
    \qmlproperty bool FolderListModel::showDotAndDotDot

    If true, the "." and ".." directories are included in the model; otherwise
    they are excluded.

    By default, this property is false.

    \sa showDirs
*/
bool QQuickFolderListModel::showDotAndDotDot() const
{
    Q_D(const QQuickFolderListModel);
    return d->showDots;
}

void  QQuickFolderListModel::setShowDotAndDotDot(bool on)
{
    Q_D(QQuickFolderListModel);

    if (on != d->showDots) {
        d->fileInfoThread.setShowDotDot(on);
    }
}

/*!
    \qmlproperty bool FolderListModel::showOnlyReadable

    If true, only readable files and directories are shown; otherwise all files
    and directories are shown.

    By default, this property is false.

    \sa showDirs
*/
bool QQuickFolderListModel::showOnlyReadable() const
{
    Q_D(const QQuickFolderListModel);
    return d->showOnlyReadable;
}

void QQuickFolderListModel::setShowOnlyReadable(bool on)
{
    Q_D(QQuickFolderListModel);

    if (on != d->showOnlyReadable) {
        d->fileInfoThread.setShowOnlyReadable(on);
    }
}

/*!
    \qmlmethod QVariant QQuickFolderListModel::get(int idx, const QString &property) const

    Get the folder property for the given index. The following properties
    are available.

    \list
        \li \c fileName
        \li \c filePath
        \li \c fileBaseName
        \li \c fileSuffix
        \li \c fileSize
        \li \c fileModified
        \li \c fileAccessed
        \li \c fileIsDir
    \endlist
*/
QVariant QQuickFolderListModel::get(int idx, const QString &property) const
{
    int role = roleFromString(property);
    if (role >= 0 && idx >= 0)
        return data(index(idx, 0), role);
    else
        return QVariant();
}

#include "moc_qquickfolderlistmodel.cpp"

//![code]
QT_END_NAMESPACE
