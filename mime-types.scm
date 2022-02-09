(define mime-types
    (alist->mapping
     string-comparator
     '(("ez" . "application/andrew-inset")
       ("anx" . "application/annodex")
       ("atom" . "application/atom+xml")
       ("atomcat" . "application/atomcat+xml")
       ("atomsrv" . "application/atomserv+xml")
       ("lin" . "application/bbolin")
       ("cu" . "application/cu-seeme")
       ("davmount" . "application/davmount+xml")
       ("dcm" . "application/dicom")
       ("tsp" . "application/dsptype")
       ("es" . "application/ecmascript")
       ("epub" . "application/epub+zip")
       ("ttf" . "application/font-sfnt")
       ("otf" . "application/font-sfnt")
       ("pfr" . "application/font-tdpfr")
       ("woff" . "application/font-woff")
       ("spl" . "application/futuresplash")
       ("gz" . "application/gzip")
       ("hta" . "application/hta")
       ("jar" . "application/java-archive")
       ("ser" . "application/java-serialized-object")
       ("class" . "application/java-vm")
       ("mjs" . "application/javascript")
       ("js" . "application/javascript")
       ("json" . "application/json")
       ("jsonld" . "application/ld+json")
       ("m3g" . "application/m3g")
       ("hqx" . "application/mac-binhex40")
       ("cpt" . "application/mac-compactpro")
       ("nbp" . "application/mathematica")
       ("nb" . "application/mathematica")
       ("mbox" . "application/mbox")
       ("mdb" . "application/msaccess")
       ("dot" . "application/msword")
       ("doc" . "application/msword")
       ("mxf" . "application/mxf")
       ("msp" . "application/octet-stream")
       ("msu" . "application/octet-stream")
       ("deploy" . "application/octet-stream")
       ("bin" . "application/octet-stream")
       ("oda" . "application/oda")
       ("opf" . "application/oebps-package+xml")
       ("ogx" . "application/ogg")
       ("onepkg" . "application/onenote")
       ("onetmp" . "application/onenote")
       ("onetoc2" . "application/onenote")
       ("one" . "application/onenote")
       ("pdf" . "application/pdf")
       ("pgp" . "application/pgp-encrypted")
       ("key" . "application/pgp-keys")
       ("sig" . "application/pgp-signature")
       ("prf" . "application/pics-rules")
       ("eps3" . "application/postscript")
       ("eps2" . "application/postscript")
       ("epsf" . "application/postscript")
       ("epsi" . "application/postscript")
       ("eps" . "application/postscript")
       ("ai" . "application/postscript")
       ("ps" . "application/postscript")
       ("rar" . "application/rar")
       ("rdf" . "application/rdf+xml")
       ("rtf" . "application/rtf")
       ("stl" . "application/sla")
       ("smil" . "application/smil+xml")
       ("smi" . "application/smil+xml")
       ("xht" . "application/xhtml+xml")
       ("xhtml" . "application/xhtml+xml")
       ("xsd" . "application/xml")
       ("xml" . "application/xml")
       ("xslt" . "application/xslt+xml")
       ("xsl" . "application/xslt+xml")
       ("xspf" . "application/xspf+xml")
       ("zip" . "application/zip")
       ("apk" . "application/vnd.android.package-archive")
       ("cdy" . "application/vnd.cinderella")
       ("udeb" . "application/vnd.debian.binary-package")
       ("ddeb" . "application/vnd.debian.binary-package")
       ("deb" . "application/vnd.debian.binary-package")
       ("sfd" . "application/vnd.font-fontforge-sfd")
       ("kml" . "application/vnd.google-earth.kml+xml")
       ("kmz" . "application/vnd.google-earth.kmz")
       ("xul" . "application/vnd.mozilla.xul+xml")
       ("xlt" . "application/vnd.ms-excel")
       ("xlb" . "application/vnd.ms-excel")
       ("xls" . "application/vnd.ms-excel")
       ("xlam" . "application/vnd.ms-excel.addin.macroEnabled.12")
       ("xlsb" . "application/vnd.ms-excel.sheet.binary.macroEnabled.12")
       ("xlsm" . "application/vnd.ms-excel.sheet.macroEnabled.12")
       ("xltm" . "application/vnd.ms-excel.template.macroEnabled.12")
       ("eot" . "application/vnd.ms-fontobject")
       ("thmx" . "application/vnd.ms-officetheme")
       ("cat" . "application/vnd.ms-pki.seccat")
       ("pps" . "application/vnd.ms-powerpoint")
       ("ppt" . "application/vnd.ms-powerpoint")
       ("ppam" . "application/vnd.ms-powerpoint.addin.macroEnabled.12")
       ("pptm" . "application/vnd.ms-powerpoint.presentation.macroEnabled.12")
       ("sldm" . "application/vnd.ms-powerpoint.slide.macroEnabled.12")
       ("ppsm" . "application/vnd.ms-powerpoint.slideshow.macroEnabled.12")
       ("potm" . "application/vnd.ms-powerpoint.template.macroEnabled.12")
       ("docm" . "application/vnd.ms-word.document.macroEnabled.12")
       ("dotm" . "application/vnd.ms-word.template.macroEnabled.12")
       ("odc" . "application/vnd.oasis.opendocument.chart")
       ("odb" . "application/vnd.oasis.opendocument.database")
       ("odf" . "application/vnd.oasis.opendocument.formula")
       ("odg" . "application/vnd.oasis.opendocument.graphics")
       ("otg" . "application/vnd.oasis.opendocument.graphics-template")
       ("odi" . "application/vnd.oasis.opendocument.image")
       ("odp" . "application/vnd.oasis.opendocument.presentation")
       ("otp" . "application/vnd.oasis.opendocument.presentation-template")
       ("ods" . "application/vnd.oasis.opendocument.spreadsheet")
       ("ots" . "application/vnd.oasis.opendocument.spreadsheet-template")
       ("odt" . "application/vnd.oasis.opendocument.text")
       ("odm" . "application/vnd.oasis.opendocument.text-master")
       ("ott" . "application/vnd.oasis.opendocument.text-template")
       ("oth" . "application/vnd.oasis.opendocument.text-web")
       ("pptx"
        .
        "application/vnd.openxmlformats-officedocument.presentationml.presentation")
       ("sldx" . "application/vnd.openxmlformats-officedocument.presentationml.slide")
       ("ppsx"
        .
        "application/vnd.openxmlformats-officedocument.presentationml.slideshow")
       ("potx"
        .
        "application/vnd.openxmlformats-officedocument.presentationml.template")
       ("xlsx" . "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
       ("xltx"
        .
        "application/vnd.openxmlformats-officedocument.spreadsheetml.template")
       ("docx"
        .
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
       ("dotx"
        .
        "application/vnd.openxmlformats-officedocument.wordprocessingml.template")
       ("cod" . "application/vnd.rim.cod")
       ("mmf" . "application/vnd.smaf")
       ("sdc" . "application/vnd.stardivision.calc")
       ("sds" . "application/vnd.stardivision.chart")
       ("sda" . "application/vnd.stardivision.draw")
       ("sdd" . "application/vnd.stardivision.impress")
       ("sdf" . "application/vnd.stardivision.math")
       ("sdw" . "application/vnd.stardivision.writer")
       ("sgl" . "application/vnd.stardivision.writer-global")
       ("sxc" . "application/vnd.sun.xml.calc")
       ("stc" . "application/vnd.sun.xml.calc.template")
       ("sxd" . "application/vnd.sun.xml.draw")
       ("std" . "application/vnd.sun.xml.draw.template")
       ("sxi" . "application/vnd.sun.xml.impress")
       ("sti" . "application/vnd.sun.xml.impress.template")
       ("sxm" . "application/vnd.sun.xml.math")
       ("sxw" . "application/vnd.sun.xml.writer")
       ("sxg" . "application/vnd.sun.xml.writer.global")
       ("stw" . "application/vnd.sun.xml.writer.template")
       ("sis" . "application/vnd.symbian.install")
       ("pcap" . "application/vnd.tcpdump.pcap")
       ("cap" . "application/vnd.tcpdump.pcap")
       ("vss" . "application/vnd.visio")
       ("vsw" . "application/vnd.visio")
       ("vst" . "application/vnd.visio")
       ("vsd" . "application/vnd.visio")
       ("wbxml" . "application/vnd.wap.wbxml")
       ("wmlc" . "application/vnd.wap.wmlc")
       ("wmlsc" . "application/vnd.wap.wmlscriptc")
       ("wpd" . "application/vnd.wordperfect")
       ("wp5" . "application/vnd.wordperfect5.1")
       ("wk" . "application/x-123")
       ("7z" . "application/x-7z-compressed")
       ("abw" . "application/x-abiword")
       ("dmg" . "application/x-apple-diskimage")
       ("bcpio" . "application/x-bcpio")
       ("torrent" . "application/x-bittorrent")
       ("cab" . "application/x-cab")
       ("cbr" . "application/x-cbr")
       ("cbz" . "application/x-cbz")
       ("cda" . "application/x-cdf")
       ("cdf" . "application/x-cdf")
       ("vcd" . "application/x-cdlink")
       ("pgn" . "application/x-chess-pgn")
       ("mph" . "application/x-comsol")
       ("cpio" . "application/x-cpio")
       ("csh" . "application/x-csh")
       ("udeb" . "application/x-debian-package")
       ("deb" . "application/x-debian-package")
       ("dxr" . "application/x-director")
       ("dir" . "application/x-director")
       ("dcr" . "application/x-director")
       ("dms" . "application/x-dms")
       ("wad" . "application/x-doom")
       ("dvi" . "application/x-dvi")
       ("gsf" . "application/x-font")
       ("pfb" . "application/x-font")
       ("pfa" . "application/x-font")
       ("pcf.Z" . "application/x-font-pcf")
       ("pcf" . "application/x-font-pcf")
       ("mm" . "application/x-freemind")
       ("spl" . "application/x-futuresplash")
       ("gan" . "application/x-ganttproject")
       ("gnumeric" . "application/x-gnumeric")
       ("sgf" . "application/x-go-sgf")
       ("gcf" . "application/x-graphing-calculator")
       ("gtar" . "application/x-gtar")
       ("taz" . "application/x-gtar-compressed")
       ("tgz" . "application/x-gtar-compressed")
       ("hdf" . "application/x-hdf")
       ("hwp" . "application/x-hwp")
       ("ica" . "application/x-ica")
       ("info" . "application/x-info")
       ("isp" . "application/x-internet-signup")
       ("ins" . "application/x-internet-signup")
       ("iii" . "application/x-iphone")
       ("iso" . "application/x-iso9660-image")
       ("jam" . "application/x-jam")
       ("jnlp" . "application/x-java-jnlp-file")
       ("jmz" . "application/x-jmol")
       ("chrt" . "application/x-kchart")
       ("kil" . "application/x-killustrator")
       ("skm" . "application/x-koan")
       ("skt" . "application/x-koan")
       ("skd" . "application/x-koan")
       ("skp" . "application/x-koan")
       ("kpt" . "application/x-kpresenter")
       ("kpr" . "application/x-kpresenter")
       ("ksp" . "application/x-kspread")
       ("kwt" . "application/x-kword")
       ("kwd" . "application/x-kword")
       ("latex" . "application/x-latex")
       ("lha" . "application/x-lha")
       ("lyx" . "application/x-lyx")
       ("lzh" . "application/x-lzh")
       ("lzx" . "application/x-lzx")
       ("fbdoc" . "application/x-maker")
       ("book" . "application/x-maker")
       ("fb" . "application/x-maker")
       ("fm" . "application/x-maker")
       ("frame" . "application/x-maker")
       ("maker" . "application/x-maker")
       ("frm" . "application/x-maker")
       ("mif" . "application/x-mif")
       ("m3u8" . "application/x-mpegURL")
       ("application" . "application/x-ms-application")
       ("manifest" . "application/x-ms-manifest")
       ("wmd" . "application/x-ms-wmd")
       ("wmz" . "application/x-ms-wmz")
       ("dll" . "application/x-msdos-program")
       ("bat" . "application/x-msdos-program")
       ("exe" . "application/x-msdos-program")
       ("com" . "application/x-msdos-program")
       ("msi" . "application/x-msi")
       ("nc" . "application/x-netcdf")
       ("pac" . "application/x-ns-proxy-autoconfig")
       ("nwc" . "application/x-nwc")
       ("o" . "application/x-object")
       ("oza" . "application/x-oz-application")
       ("p7r" . "application/x-pkcs7-certreqresp")
       ("crl" . "application/x-pkcs7-crl")
       ("pyo" . "application/x-python-code")
       ("pyc" . "application/x-python-code")
       ("shx" . "application/x-qgis")
       ("shp" . "application/x-qgis")
       ("qgs" . "application/x-qgis")
       ("qtl" . "application/x-quicktimeplayer")
       ("rdp" . "application/x-rdp")
       ("rpm" . "application/x-redhat-package-manager")
       ("rss" . "application/x-rss+xml")
       ("rb" . "application/x-ruby")
       ("sce" . "application/x-scilab")
       ("sci" . "application/x-scilab")
       ("xcos" . "application/x-scilab-xcos")
       ("sh" . "application/x-sh")
       ("shar" . "application/x-shar")
       ("swfl" . "application/x-shockwave-flash")
       ("swf" . "application/x-shockwave-flash")
       ("scr" . "application/x-silverlight")
       ("sql" . "application/x-sql")
       ("sitx" . "application/x-stuffit")
       ("sit" . "application/x-stuffit")
       ("sv4cpio" . "application/x-sv4cpio")
       ("sv4crc" . "application/x-sv4crc")
       ("tar" . "application/x-tar")
       ("tcl" . "application/x-tcl")
       ("gf" . "application/x-tex-gf")
       ("pk" . "application/x-tex-pk")
       ("texi" . "application/x-texinfo")
       ("texinfo" . "application/x-texinfo")
       ("sik" . "application/x-trash")
       ("old" . "application/x-trash")
       ("bak" . "application/x-trash")
       ("%" . "application/x-trash")
       ("~" . "application/x-trash")
       ("roff" . "application/x-troff")
       ("tr" . "application/x-troff")
       ("t" . "application/x-troff")
       ("man" . "application/x-troff-man")
       ("me" . "application/x-troff-me")
       ("ms" . "application/x-troff-ms")
       ("ustar" . "application/x-ustar")
       ("src" . "application/x-wais-source")
       ("wz" . "application/x-wingz")
       ("crt" . "application/x-x509-ca-cert")
       ("xcf" . "application/x-xcf")
       ("fig" . "application/x-xfig")
       ("xpi" . "application/x-xpinstall")
       ("xz" . "application/x-xz")
       ("amr" . "audio/amr")
       ("awb" . "audio/amr-wb")
       ("axa" . "audio/annodex")
       ("snd" . "audio/basic")
       ("au" . "audio/basic")
       ("sco" . "audio/csound")
       ("orc" . "audio/csound")
       ("csd" . "audio/csound")
       ("flac" . "audio/flac")
       ("kar" . "audio/midi")
       ("midi" . "audio/midi")
       ("mid" . "audio/midi")
       ("m4a" . "audio/mpeg")
       ("mp3" . "audio/mpeg")
       ("mp2" . "audio/mpeg")
       ("mpega" . "audio/mpeg")
       ("mpga" . "audio/mpeg")
       ("m3u" . "audio/mpegurl")
       ("spx" . "audio/ogg")
       ("opus" . "audio/ogg")
       ("ogg" . "audio/ogg")
       ("oga" . "audio/ogg")
       ("sid" . "audio/prs.sid")
       ("aifc" . "audio/x-aiff")
       ("aiff" . "audio/x-aiff")
       ("aif" . "audio/x-aiff")
       ("gsm" . "audio/x-gsm")
       ("m3u" . "audio/x-mpegurl")
       ("wma" . "audio/x-ms-wma")
       ("wax" . "audio/x-ms-wax")
       ("ram" . "audio/x-pn-realaudio")
       ("rm" . "audio/x-pn-realaudio")
       ("ra" . "audio/x-pn-realaudio")
       ("ra" . "audio/x-realaudio")
       ("pls" . "audio/x-scpls")
       ("sd2" . "audio/x-sd2")
       ("wav" . "audio/x-wav")
       ("alc" . "chemical/x-alchemy")
       ("cache" . "chemical/x-cache")
       ("cac" . "chemical/x-cache")
       ("csf" . "chemical/x-cache-csf")
       ("ctab" . "chemical/x-cactvs-binary")
       ("cascii" . "chemical/x-cactvs-binary")
       ("cbin" . "chemical/x-cactvs-binary")
       ("cdx" . "chemical/x-cdx")
       ("cer" . "chemical/x-cerius")
       ("c3d" . "chemical/x-chem3d")
       ("chm" . "chemical/x-chemdraw")
       ("cif" . "chemical/x-cif")
       ("cmdf" . "chemical/x-cmdf")
       ("cml" . "chemical/x-cml")
       ("cpa" . "chemical/x-compass")
       ("bsd" . "chemical/x-crossfire")
       ("csm" . "chemical/x-csml")
       ("csml" . "chemical/x-csml")
       ("ctx" . "chemical/x-ctx")
       ("cef" . "chemical/x-cxf")
       ("cxf" . "chemical/x-cxf")
       ("embl" . "chemical/x-embl-dl-nucleotide")
       ("emb" . "chemical/x-embl-dl-nucleotide")
       ("spc" . "chemical/x-galactic-spc")
       ("gamin" . "chemical/x-gamess-input")
       ("gam" . "chemical/x-gamess-input")
       ("inp" . "chemical/x-gamess-input")
       ("fchk" . "chemical/x-gaussian-checkpoint")
       ("fch" . "chemical/x-gaussian-checkpoint")
       ("cub" . "chemical/x-gaussian-cube")
       ("gjf" . "chemical/x-gaussian-input")
       ("gjc" . "chemical/x-gaussian-input")
       ("gau" . "chemical/x-gaussian-input")
       ("gal" . "chemical/x-gaussian-log")
       ("gcg" . "chemical/x-gcg8-sequence")
       ("gen" . "chemical/x-genbank")
       ("hin" . "chemical/x-hin")
       ("ist" . "chemical/x-isostar")
       ("istr" . "chemical/x-isostar")
       ("dx" . "chemical/x-jcamp-dx")
       ("jdx" . "chemical/x-jcamp-dx")
       ("kin" . "chemical/x-kinemage")
       ("mcm" . "chemical/x-macmolecule")
       ("mmod" . "chemical/x-macromodel-input")
       ("mmd" . "chemical/x-macromodel-input")
       ("mol" . "chemical/x-mdl-molfile")
       ("rd" . "chemical/x-mdl-rdfile")
       ("rxn" . "chemical/x-mdl-rxnfile")
       ("sdf" . "chemical/x-mdl-sdfile")
       ("sd" . "chemical/x-mdl-sdfile")
       ("tgf" . "chemical/x-mdl-tgf")
       ("mcif" . "chemical/x-mmcif")
       ("mol2" . "chemical/x-mol2")
       ("b" . "chemical/x-molconn-Z")
       ("gpt" . "chemical/x-mopac-graph")
       ("zmt" . "chemical/x-mopac-input")
       ("mpc" . "chemical/x-mopac-input")
       ("mopcrt" . "chemical/x-mopac-input")
       ("mop" . "chemical/x-mopac-input")
       ("moo" . "chemical/x-mopac-out")
       ("mvb" . "chemical/x-mopac-vib")
       ("asn" . "chemical/x-ncbi-asn1")
       ("ent" . "chemical/x-ncbi-asn1-ascii")
       ("prt" . "chemical/x-ncbi-asn1-ascii")
       ("aso" . "chemical/x-ncbi-asn1-binary")
       ("val" . "chemical/x-ncbi-asn1-binary")
       ("asn" . "chemical/x-ncbi-asn1-spec")
       ("ent" . "chemical/x-pdb")
       ("pdb" . "chemical/x-pdb")
       ("ros" . "chemical/x-rosdal")
       ("sw" . "chemical/x-swissprot")
       ("vms" . "chemical/x-vamas-iso14976")
       ("vmd" . "chemical/x-vmd")
       ("xtel" . "chemical/x-xtel")
       ("xyz" . "chemical/x-xyz")
       ("ttc" . "font/collection")
       ("otf" . "font/otf")
       ("ttf" . "font/otf")
       ("otf" . "font/sfnt")
       ("ttf" . "font/sfnt")
       ("otf" . "font/ttf")
       ("ttf" . "font/ttf")
       ("woff" . "font/woff")
       ("woff2" . "font/woff2")
       ("gif" . "image/gif")
       ("ief" . "image/ief")
       ("jpg2" . "image/jp2")
       ("jp2" . "image/jp2")
       ("jpe" . "image/jpeg")
       ("jpg" . "image/jpeg")
       ("jpeg" . "image/jpeg")
       ("jpm" . "image/jpm")
       ("jpf" . "image/jpx")
       ("jpx" . "image/jpx")
       ("pcx" . "image/pcx")
       ("png" . "image/png")
       ("svgz" . "image/svg+xml")
       ("svg" . "image/svg+xml")
       ("tif" . "image/tiff")
       ("tiff" . "image/tiff")
       ("djv" . "image/vnd.djvu")
       ("djvu" . "image/vnd.djvu")
       ("ico" . "image/vnd.microsoft.icon")
       ("wbmp" . "image/vnd.wap.wbmp")
       ("cr2" . "image/x-canon-cr2")
       ("crw" . "image/x-canon-crw")
       ("ras" . "image/x-cmu-raster")
       ("cdr" . "image/x-coreldraw")
       ("pat" . "image/x-coreldrawpattern")
       ("cdt" . "image/x-coreldrawtemplate")
       ("cpt" . "image/x-corelphotopaint")
       ("erf" . "image/x-epson-erf")
       ("art" . "image/x-jg")
       ("jng" . "image/x-jng")
       ("bmp" . "image/x-ms-bmp")
       ("nef" . "image/x-nikon-nef")
       ("orf" . "image/x-olympus-orf")
       ("psd" . "image/x-photoshop")
       ("pnm" . "image/x-portable-anymap")
       ("pbm" . "image/x-portable-bitmap")
       ("pgm" . "image/x-portable-graymap")
       ("ppm" . "image/x-portable-pixmap")
       ("rgb" . "image/x-rgb")
       ("xbm" . "image/x-xbitmap")
       ("xpm" . "image/x-xpixmap")
       ("xwd" . "image/x-xwindowdump")
       ("eml" . "message/rfc822")
       ("iges" . "model/iges")
       ("igs" . "model/iges")
       ("silo" . "model/mesh")
       ("mesh" . "model/mesh")
       ("msh" . "model/mesh")
       ("vrml" . "model/vrml")
       ("wrl" . "model/vrml")
       ("x3dv" . "model/x3d+vrml")
       ("x3d" . "model/x3d+xml")
       ("x3db" . "model/x3d+binary")
       ("appcache" . "text/cache-manifest")
       ("icz" . "text/calendar")
       ("ics" . "text/calendar")
       ("css" . "text/css")
       ("csv" . "text/csv")
       ("gmi" . "text/gemini")
       ("323" . "text/h323")
       ("shtml" . "text/html")
       ("htm" . "text/html")
       ("html" . "text/html")
       ("uls" . "text/iuls")
       ("mml" . "text/mathml")
       ("srt" . "text/plain")
       ("brf" . "text/plain")
       ("pot" . "text/plain")
       ("text" . "text/plain")
       ("txt" . "text/plain")
       ("asc" . "text/plain")
       ("rtx" . "text/richtext")
       ("wsc" . "text/scriptlet")
       ("sct" . "text/scriptlet")
       ("tm" . "text/texmacs")
       ("tsv" . "text/tab-separated-values")
       ("ttl" . "text/turtle")
       ("vcard" . "text/vcard")
       ("vcf" . "text/vcard")
       ("jad" . "text/vnd.sun.j2me.app-descriptor")
       ("wml" . "text/vnd.wap.wml")
       ("wmls" . "text/vnd.wap.wmlscript")
       ("bib" . "text/x-bibtex")
       ("boo" . "text/x-boo")
       ("hh" . "text/x-c++hdr")
       ("hxx" . "text/x-c++hdr")
       ("hpp" . "text/x-c++hdr")
       ("h++" . "text/x-c++hdr")
       ("cc" . "text/x-c++src")
       ("cxx" . "text/x-c++src")
       ("cpp" . "text/x-c++src")
       ("c++" . "text/x-c++src")
       ("h" . "text/x-chdr")
       ("htc" . "text/x-component")
       ("csh" . "text/x-csh")
       ("c" . "text/x-csrc")
       ("d" . "text/x-dsrc")
       ("patch" . "text/x-diff")
       ("diff" . "text/x-diff")
       ("hs" . "text/x-haskell")
       ("java" . "text/x-java")
       ("ly" . "text/x-lilypond")
       ("lhs" . "text/x-literate-haskell")
       ("moc" . "text/x-moc")
       ("pas" . "text/x-pascal")
       ("p" . "text/x-pascal")
       ("gcd" . "text/x-pcs-gcd")
       ("pm" . "text/x-perl")
       ("pl" . "text/x-perl")
       ("py" . "text/x-python")
       ("scala" . "text/x-scala")
       ("etx" . "text/x-setext")
       ("sfv" . "text/x-sfv")
       ("sh" . "text/x-sh")
       ("tk" . "text/x-tcl")
       ("tcl" . "text/x-tcl")
       ("cls" . "text/x-tex")
       ("sty" . "text/x-tex")
       ("ltx" . "text/x-tex")
       ("tex" . "text/x-tex")
       ("vcs" . "text/x-vcalendar")
       ("3gp" . "video/3gpp")
       ("axv" . "video/annodex")
       ("dl" . "video/dl")
       ("dv" . "video/dv")
       ("dif" . "video/dv")
       ("fli" . "video/fli")
       ("gl" . "video/gl")
       ("mpe" . "video/mpeg")
       ("mpg" . "video/mpeg")
       ("mpeg" . "video/mpeg")
       ("ts" . "video/MP2T")
       ("mp4" . "video/mp4")
       ("mov" . "video/quicktime")
       ("qt" . "video/quicktime")
       ("ogv" . "video/ogg")
       ("webm" . "video/webm")
       ("mxu" . "video/vnd.mpegurl")
       ("flv" . "video/x-flv")
       ("lsx" . "video/x-la-asf")
       ("lsf" . "video/x-la-asf")
       ("mng" . "video/x-mng")
       ("asx" . "video/x-ms-asf")
       ("asf" . "video/x-ms-asf")
       ("wm" . "video/x-ms-wm")
       ("wmv" . "video/x-ms-wmv")
       ("wmx" . "video/x-ms-wmx")
       ("wvx" . "video/x-ms-wvx")
       ("avi" . "video/x-msvideo")
       ("movie" . "video/x-sgi-movie")
       ("mkv" . "video/x-matroska")
       ("mpv" . "video/x-matroska")
       ("ice" . "x-conference/x-cooltalk")
       ("sisx" . "x-epoc/x-sisx-app")
       ("wrl" . "x-world/x-vrml")
       ("vrml" . "x-world/x-vrml")
       ("vrm" . "x-world/x-vrml"))))
