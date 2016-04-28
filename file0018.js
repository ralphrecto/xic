var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 12</i>';
states['fold000001'] = false;
texts['fold000016'] = '<a href="javascript:fold(\'fold000016\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 16 to line 16</i>';
states['fold000016'] = false;
texts['fold000026'] = '<a href="javascript:fold(\'fold000026\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 26 to line 26</i>';
states['fold000026'] = false;
texts['fold000040'] = '<a href="javascript:fold(\'fold000040\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 40 to line 42</i>';
states['fold000040'] = false;
texts['fold000044'] = '<a href="javascript:fold(\'fold000044\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 44 to line 46</i>';
states['fold000044'] = false;
texts['fold000048'] = '<a href="javascript:fold(\'fold000048\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 48 to line 49</i>';
states['fold000048'] = false;
texts['fold000052'] = '<a href="javascript:fold(\'fold000052\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 52 to line 59</i>';
states['fold000052'] = false;
texts['fold000061'] = '<a href="javascript:fold(\'fold000061\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 61 to line 70</i>';
states['fold000061'] = false;
texts['fold000072'] = '<a href="javascript:fold(\'fold000072\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 72 to line 98</i>';
states['fold000072'] = false;

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
