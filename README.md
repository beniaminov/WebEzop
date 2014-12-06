WebEzop
=======

Редактор онтологии в среде Web на Visual Prolog 5.2 для Ezop_server

В данном проекте представлены исходные файлы на языке Visual Prolog 5.2 для исполняемой программы Editor.exe  Web-сервера ЭЗОП (Ezop_server). Программа EDITOR.exe помещается в папку  xampp\htpdocs\ezop\EXE. Программа выполняет грамматический анализ текстов онтологий и вопросов, производит логическую обработку и формирует ответы на вопросы к онтологии.

Компилятор Visual Prolog 5.2 для сборки программы EDITOR.exe данного проекта можно загрузить по адресу https://sourceforge.net/projects/ezop-project/files/Visual%20Prolog%205.2/VisualProlog_5_2/Vip52.zip/download 

Отладку программы Editor.exe можно выполнить с помощью предиката send_to_debug(string), который отправляет сообщения в файл
debugging_file.txt.

Система Элементов Задач и ОПределений (ЭЗОП)представляет собой Web-сервер коллективного конструирования библиотек онтологий. Система должна работать в стиле Web 2.0.

Web-сервер онтологий предполагает многопользовательскую работу с онтологиями, когда необходимо обеспечить пользователям возможность формировать на сервере разделы библиотек онтологий для последующего общего использования в среде Интернет.

Cервер разработан с использованием Drupal 5.

Для установки сервера нужно воспользоваться программой xampp под windows, совместимой с Drupal 5. Папку ezop настоящего репозитория нужно разместить в папке xampp\htdocs на компьютере сервера. В файле xampp\apache\conf\httpd.conf нужно указать доступность файлов с расширением .exe для Exec_CGI. Для запуска сервера нужно открыть панель xampp в папке xampp, и в панели запустить Apache и MySQL. Сервер будет доступен через браузер по адресу http://адрес_сервера/ezop.

Текущая версия сервера онтологий доступна по адресу http://ontoserver.rsuh.ru.


