
/***************** DLL EXPORT MAKRO FOR TCC AND GCC ************/
#if (defined(_WIN32) && (defined(_MSC_VER)|| defined(__TINYC__)  || (defined(__BORLANDC__) && (__BORLANDC__ >= 0x0550)) || defined(__LCC__) || defined(__WATCOMC__) || (defined(__GNUC__) && defined(__declspec))))
#undef DLLIMPORT
#undef DLLEXPORT
#   define DLLIMPORT __declspec(dllimport)
#   define DLLEXPORT __declspec(dllexport)
#else
#undef DLLIMPORT
#undef DLLEXPORT
#   define DLLIMPORT 
#   if defined(__GNUC__) && __GNUC__ > 3
#       define DLLEXPORT __attribute__ ((visibility("default")))
#   else
#       define DLLEXPORT
#   endif
#endif
/***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

DLLEXPORT
int ok(int a) {return a;};
// comment

typedef struct  {
    int uid;
    char* text;
    int len;
} Phrase;
typedef struct  {
    int uid;
    char* text;
    int len;
} Phrase2;


DLLEXPORT Phrase* makestruct(int uid, const char* text) {
    Phrase* mystruct;
    mystruct=malloc(sizeof(Phrase)+1);
    mystruct->uid=uid;
    mystruct->text=strdup(text);
    mystruct->len=strlen(text);
    //printf("makestruct: p %p p.text %p\n",mystruct, mystruct->text);
    return mystruct;
};
DLLEXPORT Phrase* makestruct2(int uid, const char* text2) {
    Phrase* mystruct;
    mystruct=malloc(sizeof(Phrase));
    mystruct->uid=uid;
    mystruct->text=strdup(text2);
    mystruct->len=strlen(text2);
    //printf("makestruct2: p %p p.text %p\n",mystruct, mystruct->text);
    return mystruct;
};
DLLEXPORT char* printstruct(Phrase* phr) {
    if(phr==NULL) return "error";
    printf("printstruct: phr %p phr.text %p (%s)\n",phr,phr->text,phr->text);
    return strdup(phr->text);
};

DLLEXPORT int freestruct(Phrase* phr) {
    if(phr==NULL) return -1;
    free(phr->text);
    free(phr);
    return 0;
}

// try a callback from tcl
DLLEXPORT void printstruct_cb (Phrase* phr, void (printphrase_cb)(Phrase* aphr)) {
    printphrase_cb(phr);
}


DLLEXPORT int test(void) {
    Phrase* astruct;
    astruct= makestruct(0,"test ok");
    char* txt=printstruct(astruct);
    printf("result: %s\n",txt);
    freestruct(astruct);
    return 0;
};

#undef DLLIMPORT
#undef DLLEXPORT
