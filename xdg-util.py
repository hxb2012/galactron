#!/usr/bin/env python3

import os
from os.path import dirname, join, expanduser
import sys
from gi.repository import Gio

def get_emacsclient(app, args=""):
    assert app.get_string('NoDisplay') != 'true', "emacs started from org-protocol"
    assert 'emacsclient' not in app.get_keywords(), "emacs started from emacsclient"
    executable = app.get_executable()
    if not app.has_key("X-Flatpak"):
        return join(dirname(executable), "emacsclient") + f" -a '{executable}{args}'"
    flatpak_id = app.get_string("X-Flatpak")
    return f"{executable} run --command=emacsclient {flatpak_id} -a 'emacs-wrapper{args}'"

def open_xdg_application(filename):
    sharedir = os.environ.get("XDG_DATA_HOME", expanduser("~/.local/share"))
    path = join(sharedir, "applications", filename)
    return open(path, 'x')

def ensure_org_protocol(path):
    app = Gio.DesktopAppInfo.get_default_for_type('x-scheme-handler/org-protocol', True)
    if app:
        return False, app.get_filename()

    app = Gio.DesktopAppInfo.new_from_filename(path)
    executable = get_emacsclient(app, " --eval (orb-capture-clip--detect-protocol)")
    icon = app.get_icon().to_string()

    data = f"""[Desktop Entry]
Name=Org-Protocol
Exec={executable} -r %u
Icon={icon}
Type=Application
Terminal=false
MimeType=x-scheme-handler/org-protocol
NoDisplay=true
"""
    with open_xdg_application("org-protocol.desktop") as f:
        f.write(data)

    return True, f.name

def ensure_emacsclient(path):
    for app in Gio.DesktopAppInfo.get_all():
        if 'emacsclient' in app.get_keywords():
            return False, app.get_filename()

    app = Gio.DesktopAppInfo.new_from_filename(path)
    executable = get_emacsclient(app)

    icon = app.get_icon().to_string()
    data = f"""[Desktop Entry]
Name={app.get_name()} (Client)
GenericName={app.get_generic_name()}
Comment={app.get_description()}
MimeType={"".join(f"{t};" for t in app.get_supported_types())}
Exec={executable} -r %F
Icon={icon}
Type={app.get_string('Type')}
Terminal={app.get_string('Terminal')}
Categories={app.get_categories()}
StartupNotify={app.get_string('StartupNotify')}
StartupWMClass={app.get_startup_wm_class()}
Keywords=emacsclient;
"""

    with open_xdg_application("emacsclient.desktop") as f:
        f.write(data)

    return True, f.name

if __name__ == '__main__':
    assert len(sys.argv) > 2
    if sys.argv[1] == 'org-protocol':
        created, filename = ensure_org_protocol(sys.argv[2])
    elif sys.argv[1] == 'emacsclient':
        created, filename = ensure_emacsclient(sys.argv[2])
    else:
        assert False

    print("created" if created else "already exist", file=sys.stderr)
    print(filename)
