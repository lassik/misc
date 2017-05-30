#!/usr/bin/python

# Adapted from urwid/examples/pop_up.py

# How can anyone make it so complicated to show a pop-up dialog?

import sys

import urwid


g_email = ''


def is_valid_email(s):
    return '@' in s


class ErrorDialog(urwid.WidgetWrap):
    signals = ['close']

    def __init__(self, error_message):
        ok_button = urwid.Button('OK')
        urwid.connect_signal(
            ok_button, 'click', lambda _: self._emit('close'))
        pile = urwid.Pile(
            [urwid.Text(error_message),
             urwid.Divider(),
             ok_button])
        fill = urwid.Filler(pile)
        self.__super.__init__(urwid.AttrWrap(fill, 'popbg'))


class MyDialog(urwid.PopUpLauncher):
    def __init__(self):
        self.email_edit = urwid.Edit('Email: ')
        ok_button = urwid.Button('OK')
        urwid.connect_signal(ok_button, 'click', self.ok_button_click)
        pile = urwid.Pile([self.email_edit, urwid.Divider(), ok_button])
        self.__super.__init__(pile)

    def ok_button_click(self, _):
        global g_email
        email = self.email_edit.get_edit_text()
        if not is_valid_email(email):
            return self.show_error('The email address is not valid')
        g_email = email
        raise urwid.ExitMainLoop()

    def show_error(self, error_message):
        self.error_message = error_message
        self.open_pop_up()
        return None

    def create_pop_up(self):
        pop_up = ErrorDialog(self.error_message)
        urwid.connect_signal(
            pop_up, 'close', lambda button: self.close_pop_up())
        return pop_up

    def get_pop_up_parameters(self):
        return {'left': 0, 'top': 1, 'overlay_width': 32, 'overlay_height': 7}


if __name__ == '__main__':
    fill = urwid.Filler(urwid.Padding(MyDialog(), 'center', 15))
    try:
        urwid.MainLoop(fill, [('popbg', 'white', 'dark blue')],
                       pop_ups=True).run()
    except KeyboardInterrupt:
        sys.exit('Interrupted')
    print(g_email)
