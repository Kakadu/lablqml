#ifndef CAML_PROPERTY_MAP_H
#define CAML_PROPERTY_MAP_H

#include <QtQml/QQmlPropertyMap>
#include <caml/mlvalues.h>

// by some reason I can't use `value`. Need to investigate.
typedef intnat value_hack;

class CamlPropertyMap : public QQmlPropertyMap
{
    Q_OBJECT
private:
    value_hack _saved_callback;
public:
    CamlPropertyMap();
    ~CamlPropertyMap();

    void saveCallback(value_hack _cb);
};

#endif // CAML_PROPERTY_MAP_H
