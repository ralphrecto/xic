var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 12</i>';
states['fold000001'] = false;
texts['fold000015'] = '<a href="javascript:fold(\'fold000015\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 15 to line 20</i>';
states['fold000015'] = false;
texts['fold000032'] = '<a href="javascript:fold(\'fold000032\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 32 to line 32</i>';
states['fold000032'] = false;
texts['fold000034'] = '<a href="javascript:fold(\'fold000034\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 34 to line 37</i>';
states['fold000034'] = false;
texts['fold000039'] = '<a href="javascript:fold(\'fold000039\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 39 to line 41</i>';
states['fold000039'] = false;
texts['fold000043'] = '<a href="javascript:fold(\'fold000043\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 43 to line 44</i>';
states['fold000043'] = false;
texts['fold000047'] = '<a href="javascript:fold(\'fold000047\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 47 to line 65</i>';
states['fold000047'] = false;
texts['fold000068'] = '<a href="javascript:fold(\'fold000068\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 68 to line 72</i>';
states['fold000068'] = false;
texts['fold000075'] = '<a href="javascript:fold(\'fold000075\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 75 to line 79</i>';
states['fold000075'] = false;
texts['fold000081'] = '<a href="javascript:fold(\'fold000081\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 81 to line 83</i>';
states['fold000081'] = false;
texts['fold000085'] = '<a href="javascript:fold(\'fold000085\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 85 to line 93</i>';
states['fold000085'] = false;

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
