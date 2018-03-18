#include <QtCore/QAbstractItemModel>

QModelIndex make_qmodelindex4(int x, int y, void* ptr, const QAbstractItemModel *amodel) {
    return QModelIndex(x,y,ptr,amodel);
}

