SchemeToDo
===========
ToDoを管理するスクリプト

使い方
------
ToDo追加
-------
`:w`

	>>> :w
	ToDo> Haskeる
	Date> 2012/5/1
	[1]|2012/5/1|: Haskeる
	-> 残り40日です。

+ 残り日数が表示される。
+ 日付が過ぎている場合は **期限が過ぎていきます** と表示される。
+ 日付が当時の場合は **期限日です。** と表示される。

ToDo一覧
-------
`:r`

	>>> :r
	[1]|2012/5/1|: Haskeる
	-> 残り40日です。
	[2]|2012/5/2|: Schemeる
	-> 残り41日です。

+ ない場合は **Nothing** が表示される

ToDo削除
-------
`:del`

	>>> :del
	> 1
	[2]|2012/5/2|: Schemeる
	-> 残り41日です。

+ 削除したいToDoの番号を指定数する

直すところ
----------
+ [(1)]と表示されてしまうのでそこをなおす

バージョン
--------
### Ver 1.0 ###
バージョンを設定したほうがなんかソレっぽいので
**1.0** としよう！

### Ver 1.1 ###
バージョンを設定したほうがソレっぽいので
delが使えるような気がするこれを **1.1** としよう！

+ delを直した

### Ver 1.2 ###
+ 番号を直した
