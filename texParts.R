texHeader1 <- 
  "\\documentclass[12pt, a4paper]{scrreprt}\n
  \\usepackage[dvipsnames]{xcolor}\n
  \\definecolor{uiored}{cmyk}{0, 1, 1, 0} \n
  \\definecolor{uiogrey}{cmyk}{0, 0.05, 0.1, 0.29} \n
  \\definecolor{frcol}{gray}{0.5}
  \\definecolor{bgcol}{gray}{0.9}
  \\usepackage{colortbl}\n
  \\usepackage{longtable}\n
  \\usepackage{hyperref}\n
  \\usepackage{booktabs}\n
  \\usepackage{graphicx}\n
  \\usepackage{float}\n
  \\usepackage{helvet}\n
  \\usepackage[lining,scaled=.95]{ebgaramond} \n"

texHeader2 <- 
  "\\usepackage{parskip}\n
  \\usepackage{dcolumn}\n
  \\usepackage{tabularx}\n
  \\usepackage{adjustbox}\n
  \\usepackage[norsk, english]{babel} \n
  \\usepackage[left=2cm,right=2cm,top=2cm,bottom=2cm]{geometry} \n

  \\setkomafont{title}{\\color{uiored} \\normalfont \\sffamily } \n
  \\setkomafont{chapter}{\\color{uiored} \\normalfont \\sffamily \\Huge} \n
  \\setkomafont{section}{\\color{uiored} \\normalfont \\Large} \n
  \\setkomafont{subsection}{\\color{uiored} \\normalfont \\large} \n
  \\setkomafont{subsubsection}{\\color{uiored} \\normalfont \\large \\textit } \n
  \\setkomafont{caption}{\\sffamily\\bfseries} \n
  \\setkomafont{captionlabel}{\\sffamily\\bfseries } \n
  \\addtokomafont{chapterentry}{\\normalfont \\color{uiored} \\large}\n
  
  \\newlength\\myframewidth
  \\newlength\\myboxwidth
  \\setlength\\myframewidth{1em}
  
  \\newcommand*\\leftframe[2][\\linewidth]{%
    \\ifhmode\\par\\fi
    \\medskip
    \\begingroup
    \\setlength\\fboxsep{0pt}%
    \\noindent\\colorbox{uiored}{%
      \\parbox{#1}{%
        \\setlength\\fboxsep{1ex}%
        \\setlength\\myboxwidth{\\linewidth}%
        \\addtolength\\myboxwidth{-\\myframewidth}%
        \\addtolength\\myboxwidth{-2\\fboxsep}%
        \\hfill\\colorbox{uiogrey!20}{%
          \\parbox{\\myboxwidth}{#2}%
        }%
      }%
    }%
    \\endgroup
    \\medskip
  }%\\leftframe[Textbreite]{Text}\n\r
  \\begin{document}\n\r"