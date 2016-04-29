var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 7</i>';
states['fold000001'] = false;
texts['fold000010'] = '<a href="javascript:fold(\'fold000010\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 10 to line 12</i>';
states['fold000010'] = false;
texts['fold000041'] = '<a href="javascript:fold(\'fold000041\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 41 to line 42</i>';
states['fold000041'] = false;
texts['fold000044'] = '<a href="javascript:fold(\'fold000044\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 44 to line 44</i>';
states['fold000044'] = false;
texts['fold000046'] = '<a href="javascript:fold(\'fold000046\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 46 to line 46</i>';
states['fold000046'] = false;
texts['fold000048'] = '<a href="javascript:fold(\'fold000048\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 48 to line 49</i>';
states['fold000048'] = false;
texts['fold000052'] = '<a href="javascript:fold(\'fold000052\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 52 to line 52</i>';
states['fold000052'] = false;
texts['fold000069'] = '<a href="javascript:fold(\'fold000069\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 69 to line 70</i>';
states['fold000069'] = false;
texts['fold000074'] = '<a href="javascript:fold(\'fold000074\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 74 to line 74</i>';
states['fold000074'] = false;
texts['fold000076'] = '<a href="javascript:fold(\'fold000076\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 76 to line 77</i>';
states['fold000076'] = false;
texts['fold000081'] = '<a href="javascript:fold(\'fold000081\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 81 to line 81</i>';
states['fold000081'] = false;
texts['fold000083'] = '<a href="javascript:fold(\'fold000083\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 83 to line 84</i>';
states['fold000083'] = false;
texts['fold000087'] = '<a href="javascript:fold(\'fold000087\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 87 to line 87</i>';
states['fold000087'] = false;
texts['fold000093'] = '<a href="javascript:fold(\'fold000093\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 93 to line 93</i>';
states['fold000093'] = false;
texts['fold000095'] = '<a href="javascript:fold(\'fold000095\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 95 to line 95</i>';
states['fold000095'] = false;

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
