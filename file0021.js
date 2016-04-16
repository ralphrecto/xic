var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 17</i>';
states['fold000001'] = false;
texts['fold000020'] = '<a href="javascript:fold(\'fold000020\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 20 to line 29</i>';
states['fold000020'] = false;
texts['fold000031'] = '<a href="javascript:fold(\'fold000031\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 31 to line 38</i>';
states['fold000031'] = false;
texts['fold000040'] = '<a href="javascript:fold(\'fold000040\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 40 to line 44</i>';
states['fold000040'] = false;
texts['fold000048'] = '<a href="javascript:fold(\'fold000048\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 48 to line 48</i>';
states['fold000048'] = false;
texts['fold000054'] = '<a href="javascript:fold(\'fold000054\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 54 to line 55</i>';
states['fold000054'] = false;
texts['fold000058'] = '<a href="javascript:fold(\'fold000058\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 58 to line 62</i>';
states['fold000058'] = false;
texts['fold000065'] = '<a href="javascript:fold(\'fold000065\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 65 to line 66</i>';
states['fold000065'] = false;
texts['fold000068'] = '<a href="javascript:fold(\'fold000068\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 68 to line 68</i>';
states['fold000068'] = false;
texts['fold000070'] = '<a href="javascript:fold(\'fold000070\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 70 to line 70</i>';
states['fold000070'] = false;
texts['fold000075'] = '<a href="javascript:fold(\'fold000075\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 75 to line 75</i>';
states['fold000075'] = false;
texts['fold000078'] = '<a href="javascript:fold(\'fold000078\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 78 to line 80</i>';
states['fold000078'] = false;
texts['fold000086'] = '<a href="javascript:fold(\'fold000086\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 86 to line 86</i>';
states['fold000086'] = false;
texts['fold000091'] = '<a href="javascript:fold(\'fold000091\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 91 to line 91</i>';
states['fold000091'] = false;
texts['fold000097'] = '<a href="javascript:fold(\'fold000097\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 97 to line 97</i>';
states['fold000097'] = false;
texts['fold000103'] = '<a href="javascript:fold(\'fold000103\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 103 to line 103</i>';
states['fold000103'] = false;

function fold(id) {
  tmp = document.getElementById(id).innerHTML;
  document.getElementById(id).innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
