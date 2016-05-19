\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hw}
\LoadClass[12pt]{article}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Imports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage[compact]{titlesec}
\RequirePackage[english]{babel}
\RequirePackage[hidelinks]{hyperref}
\RequirePackage[letterpaper,margin=1in]{geometry}
\RequirePackage{enumerate}
\RequirePackage{fancyhdr}
\RequirePackage{float}
\RequirePackage{graphicx}
\RequirePackage{lastpage}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Page Layout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\setlength\parindent{1em}
\addtolength\parskip{0.1cm}
\setlength\fboxrule{.5mm}\setlength{\fboxsep}{1.2mm}
\newlength\boxlength\setlength\boxlength\textwidth
\addtolength\boxlength{-4mm}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Header and Footer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Title
\newcommand{\TITLE}{}
\renewcommand{\title}[1]{\renewcommand{\TITLE}{#1}}

% Date
\newcommand{\DATE}{\today}
\renewcommand{\date}[1]{\renewcommand{\DATE}{#1}}

% Author
\newcommand{\AUTHOR}{Seung Hee Han (sh969), Alice Meng (aym27), \\
                     Ralph Recto (rjr288), Michael Whittaker (mjw297)}
\renewcommand{\author}[1]{\renewcommand{\AUTHOR}{#1}}

% Format header and Footer
\renewcommand{\lhead}[2][L]{\fancyhead[#1]{\footnotesize{#2}}}
\renewcommand{\chead}[2][C]{\fancyhead[#1]{\footnotesize{#2}}}
\renewcommand{\rhead}[2][R]{\fancyhead[#1]{\footnotesize{#2}}}
\renewcommand{\lfoot}[2][L]{\fancyfoot[#1]{\footnotesize{#2}}}
\renewcommand{\cfoot}[2][C]{\fancyfoot[#1]{\footnotesize{#2}}}
\renewcommand{\rfoot}[2][R]{\fancyfoot[#1]{\footnotesize{#2}}}
\renewcommand{\headrulewidth}{1pt}
\renewcommand{\footrulewidth}{0pt}
\pagestyle{fancy}
\lhead{\TITLE{}}
\chead{}
\rhead{\AUTHOR{}}
\lfoot{}
\cfoot{}
\rfoot{\thepage{} of \pageref{LastPage}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Title
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\renewcommand\maketitle{
  \begin{center}
    {\Large \textbf{\TITLE}}\\
    \AUTHOR\\
    \textit{\DATE}\\
  \end{center}
}
