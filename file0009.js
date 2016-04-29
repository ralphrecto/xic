var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 13</i>';
states['fold000001'] = false;
texts['fold000015'] = '<a href="javascript:fold(\'fold000015\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 15 to line 16</i>';
states['fold000015'] = false;
texts['fold000018'] = '<a href="javascript:fold(\'fold000018\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 18 to line 33</i>';
states['fold000018'] = false;
texts['fold000035'] = '<a href="javascript:fold(\'fold000035\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 35 to line 52</i>';
states['fold000035'] = false;
texts['fold000054'] = '<a href="javascript:fold(\'fold000054\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 54 to line 59</i>';
states['fold000054'] = false;

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
