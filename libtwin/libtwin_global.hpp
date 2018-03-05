#ifndef LIBTWIN_GLOBAL_HPP
#define LIBTWIN_GLOBAL_HPP


#ifdef _WIN32 || _WIN64
    #define Q_DECL_EXPORT __declspec(dllexport)
    #define Q_DECL_IMPORT __declspec(dllimport)
#else
    #define Q_DECL_EXPORT
    #define Q_DECL_IMPORT
#endif

#if defined(LIBTWIN_LIBRARY)
#  define LIBTWINSHARED_EXPORT Q_DECL_EXPORT
#else
#  define LIBTWINSHARED_EXPORT Q_DECL_IMPORT
#endif

#endif // LIBTWIN_GLOBAL_HPP
