A port of pdfminer to common lisp.

This is essentially a direct port of parts of the python library "pdfminer" to common-lisp.

It is not-quite usefull as of yet.

The main entrypoint is process-pdf.  It requires a device to render the PDF to.  See "simple-device.lisp" for an example.  With that file loaded, this:

(pdfparse::process-pdf (pdfparse::make-pdf-resource-manager) (make-instance 'simple-pdf-device::simple-pdf-device :output-stream *standard-output*) (pdfparse::make-pdf-input-stream #p"/path/to/pdf")))

Should dump the text from the PDF to *standard-output*
