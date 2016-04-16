var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 73</i>';
states['fold000001'] = false;
texts['fold000075'] = '<a href="javascript:fold(\'fold000075\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 75 to line 80</i>';
states['fold000075'] = false;
texts['fold000082'] = '<a href="javascript:fold(\'fold000082\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 82 to line 83</i>';
states['fold000082'] = false;
texts['fold000085'] = '<a href="javascript:fold(\'fold000085\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 85 to line 97</i>';
states['fold000085'] = false;
texts['fold000099'] = '<a href="javascript:fold(\'fold000099\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 99 to line 100</i>';
states['fold000099'] = false;
texts['fold000102'] = '<a href="javascript:fold(\'fold000102\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 102 to line 104</i>';
states['fold000102'] = false;

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
