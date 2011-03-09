#include <errno.h>
#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

CAMLprim value quiet_close(value fd_v)
{
    CAMLparam1(fd_v);
    int fd = Int_val(fd_v);
    do {
        close(fd);
    } while (errno == EINTR);
    CAMLreturn(Val_unit);
}
