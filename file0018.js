var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 7</i>';
states['fold000001'] = false;
texts['fold000010'] = '<a href="javascript:fold(\'fold000010\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 10 to line 12</i>';
states['fold000010'] = false;
texts['fold000015'] = '<a href="javascript:fold(\'fold000015\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 15 to line 15</i>';
states['fold000015'] = false;
texts['fold000036'] = '<a href="javascript:fold(\'fold000036\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 36 to line 37</i>';
states['fold000036'] = false;
texts['fold000039'] = '<a href="javascript:fold(\'fold000039\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 39 to line 39</i>';
states['fold000039'] = false;
texts['fold000041'] = '<a href="javascript:fold(\'fold000041\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 41 to line 41</i>';
states['fold000041'] = false;
texts['fold000043'] = '<a href="javascript:fold(\'fold000043\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 43 to line 44</i>';
states['fold000043'] = false;
texts['fold000047'] = '<a href="javascript:fold(\'fold000047\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 47 to line 47</i>';
states['fold000047'] = false;
texts['fold000064'] = '<a href="javascript:fold(\'fold000064\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 64 to line 65</i>';
states['fold000064'] = false;
texts['fold000069'] = '<a href="javascript:fold(\'fold000069\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 69 to line 69</i>';
states['fold000069'] = false;
texts['fold000071'] = '<a href="javascript:fold(\'fold000071\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 71 to line 72</i>';
states['fold000071'] = false;
texts['fold000076'] = '<a href="javascript:fold(\'fold000076\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 76 to line 76</i>';
states['fold000076'] = false;
texts['fold000078'] = '<a href="javascript:fold(\'fold000078\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 78 to line 79</i>';
states['fold000078'] = false;
texts['fold000082'] = '<a href="javascript:fold(\'fold000082\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 82 to line 82</i>';
states['fold000082'] = false;
texts['fold000088'] = '<a href="javascript:fold(\'fold000088\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 88 to line 88</i>';
states['fold000088'] = false;
texts['fold000090'] = '<a href="javascript:fold(\'fold000090\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 90 to line 90</i>';
states['fold000090'] = false;

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
