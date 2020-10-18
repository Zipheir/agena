(module
 (gemini serve)
 ()

 (import (chicken base)
         (chicken io)
         (sendfile)
         (uri-generic))

 (include "gemini-serve-impl.scm"))
