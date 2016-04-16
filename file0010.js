var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 53</i>';
states['fold000001'] = false;
texts['fold000055'] = '<a href="javascript:fold(\'fold000055\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 55 to line 55</i>';
states['fold000055'] = false;
texts['fold000060'] = '<a href="javascript:fold(\'fold000060\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 60 to line 60</i>';
states['fold000060'] = false;
texts['fold000068'] = '<a href="javascript:fold(\'fold000068\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 68 to line 69</i>';
states['fold000068'] = false;
texts['fold000088'] = '<a href="javascript:fold(\'fold000088\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 88 to line 124</i>';
states['fold000088'] = false;
texts['fold000143'] = '<a href="javascript:fold(\'fold000143\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 143 to line 144</i>';
states['fold000143'] = false;
texts['fold000156'] = '<a href="javascript:fold(\'fold000156\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 156 to line 157</i>';
states['fold000156'] = false;
texts['fold000169'] = '<a href="javascript:fold(\'fold000169\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 169 to line 170</i>';
states['fold000169'] = false;

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
