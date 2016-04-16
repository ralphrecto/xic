var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 15</i>';
states['fold000001'] = false;
texts['fold000034'] = '<a href="javascript:fold(\'fold000034\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 34 to line 35</i>';
states['fold000034'] = false;
texts['fold000037'] = '<a href="javascript:fold(\'fold000037\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 37 to line 40</i>';
states['fold000037'] = false;
texts['fold000042'] = '<a href="javascript:fold(\'fold000042\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 42 to line 45</i>';
states['fold000042'] = false;
texts['fold000051'] = '<a href="javascript:fold(\'fold000051\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 51 to line 52</i>';
states['fold000051'] = false;
texts['fold000054'] = '<a href="javascript:fold(\'fold000054\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 54 to line 54</i>';
states['fold000054'] = false;
texts['fold000056'] = '<a href="javascript:fold(\'fold000056\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 56 to line 56</i>';
states['fold000056'] = false;
texts['fold000061'] = '<a href="javascript:fold(\'fold000061\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 61 to line 61</i>';
states['fold000061'] = false;
texts['fold000065'] = '<a href="javascript:fold(\'fold000065\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 65 to line 67</i>';
states['fold000065'] = false;
texts['fold000069'] = '<a href="javascript:fold(\'fold000069\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 69 to line 70</i>';
states['fold000069'] = false;
texts['fold000072'] = '<a href="javascript:fold(\'fold000072\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 72 to line 73</i>';
states['fold000072'] = false;
texts['fold000075'] = '<a href="javascript:fold(\'fold000075\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 75 to line 77</i>';
states['fold000075'] = false;

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
