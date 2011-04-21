/* $Id: qwrappers.h,v 1.5 1999/02/01 08:04:30 garrigue Exp $ */

#define Val_QObject(obj) ((value)(obj))
#define QObject_val(obj) ((QObject*)(obj))
#define Val_new(obj) (Val_QObject(new obj))

#define Val_QWidget(obj) ((value)(obj))
#define QWidget_val(obj) ((QWidget*)(obj))

#define Default_val(val,unwrap,default) \
(val>0 ? unwrap(Field(val,0)) : default)
#define Default_val_cast(val, unwrap, cast, default) \
(val>0 ? ((cast)(unwrap (Field(val,0)) )) : default)
#define Option_val(val,unwrap) \
((long)val-1 ? unwrap(Field(val,0)) : 0)

/* Wrapper generators */

#define ML_0(cname, conv) \
value ml_##cname##0 (value unit) { return conv (cname ()); }

#define ML_1(cname, conv1, conv) \
value ml_##cname##1 (value arg1) { return conv (cname (conv1 (arg1))); }

#define ML_2(cname, conv1, conv2, conv) \
value ml_##cname##2 (value arg1, value arg2) \
{ return conv (cname (conv1(arg1), conv2(arg2))); }
#define ML_3(cname, conv1, conv2, conv3, conv) \
value ml_##cname##3 (value arg1, value arg2, value arg3) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3))); }
#define ML_4(cname, conv1, conv2, conv3, conv4, conv) \
value ml_##cname##4 (value arg1, value arg2, value arg3, value arg4) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4))); }
#define ML_5(cname, conv1, conv2, conv3, conv4, conv5, conv) \
value ml_##cname##5 (value arg1, value arg2, value arg3, value arg4, value arg5) \
{ return conv (cname (conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), \
		      conv5(arg5))); }

#define MLmeth_0(type,name,conv) \
value ml_##type##_##name##1 (value self) \
{ return conv(((type*)QObject_val(self))->name()); }

#define MLmeth_0_dbg(type,name,conv,msg) \
value ml_##type##_##name##1 (value self) \
{ msg ; return conv(((type*)QObject_val(self))->name()); }

#define MLmeth_1(type,name,conv1,conv) \
value ml_##type##_##name##2 (value self, value arg1) \
{ return conv(((type*)QObject_val(self))->name(conv1(arg1))); }
#define MLmeth_2(type,name,conv1,conv2,conv) \
value ml_##type##_##name##3 (value self, value arg1, value arg2) \
{ return conv(((type*)QObject_val(self))->name(conv1(arg1),conv2(arg2))); }

#define MLmeth_3(type,name,conv1,conv2,conv3,conv) \
value ml_##type##_##name##4 (value self, value arg1, value arg2, value arg3) \
{ return conv(((type*)QObject_val(self))-> \
	      name(conv1(arg1),conv2(arg2),conv3(arg3))); }
#define MLmeth_4(type,name,conv1,conv2,conv3,conv4,conv) \
value ml_##type##_##name##5 (value self, value arg1, value arg2, value arg3, \
                          value arg4) \
{ return conv(((type*)QObject_val(self))-> \
	      name(conv1(arg1),conv2(arg2),conv3(arg3),conv4(arg4))); }

/* Use with care: needs the argument index */
#define Ignore(x)
#define Insert(x) x,
#define Split(x,f,g) f(x), g(x) Ignore
#define Split3(x,f,g,h) f(x), g(x), h(x) Ignore
#define Pair(x,f,g) f(Field(x,0)), g(Field(x,1)) Ignore
#define Triple(x,f,g,h) f(Field(x,0)), g(Field(x,1)), h(Field(x,2)) Ignore

/* result conversion */
#define Unit(x) ((x), Val_unit)
#define Val_char Val_int
#define Val_any(any) ((value) any)

#define QString_val(x) (QString(String_val(x)))

#define Val_none Val_int(0)
