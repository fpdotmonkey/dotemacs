import datetime
import platform
import socket
import subprocess
import sys

import dbus


def branch_name():
    hostname = socket.gethostname()
    suffix = ""
    match platform.system():
        case "Windows":
            suffix = "-win"
        case "Darwin":
            suffix = "-osx"
    return hostname + suffix


def current_branch():
    return subprocess.run(
        ["git", "rev-parse", "--abbrev-ref", "HEAD"], capture_output=True
    ).stdout


def commit_message():
    current_datetime = datetime.datetime.now().isoformat()
    return "Automatic update on " + current_datetime


def committable_changes():
    git_status = subprocess.run(
        ["git", "status", "--porcelain"], capture_output=True
    ).stdout
    return len(git_status.strip()) > 0


def mergeable_on_master():
    git_log = subprocess.run(
        ["git", "log", "master", "^" + branch_name(), "--oneline"],
        capture_output=True,
    )
    return len(git_log.strip()) > 0


def mergeable_on_branch():
    git_log = subprocess.run(
        ["git", "log", branch_name(), "^master", "--oneline"],
        capture_output=True,
    )
    return len(git_log.strip()) > 0


def merge(branch: str) -> bool:
    success = subprocess.run(["git", "merge", branch]).returncode == 0
    if not success:
        subprocess.run(["git", "merge", "--abort"])
    return success


def notify_merge_conflict(head, merging):
    bus = dbus.SessionBus()
    notifications = bus.get_object(
        "org.freedesktop.Notifications", "/org/freedesktop/Notifications"
    )
    iface = dbus.Interface(
        notifications, dbus_interface="org.freedesktop.Notifications"
    )
    iface.Notify(
        ".emacs auto merger",
        0,
        "emacs",
        ".emacs merge conflict",
        f"Failed to merge {merging} into {head}",
        [],
        {"desktop-entry": "emacs.desktop", "urgency": 1},
        -1,
    )


def notify_failed_checkout(branch):
    bus = dbus.SessionBus()
    notifications = bus.get_object(
        "org.freedesktop.Notifications", "/org/freedesktop/Notifications"
    )
    iface = dbus.Interface(
        notifications, dbus_interface="org.freedesktop.Notifications"
    )
    iface.Notify(
        ".emacs auto merger",
        0,
        "emacs",
        "Branch checkout",
        f"Failed to checkout {branch}",
        [],
        {"desktop-entry": "emacs.desktop", "urgency": 1},
        -1,
    )


def main():
    if (
        current_branch() != branch_name()
        and subprocess.run(["git", "checkout", "-B", branch_name()]).returncode
        != 0
    ):
        notify_failed_checkout(branch_name())
        return
    if committable_changes():
        print("committing changes")
        subprocess.run(["git", "add", "."])
        subprocess.run(["git", "commit", "-m", commit_message()])
    if mergeable_on_master():
        print("merging master into " + current_branch())
        if not merge("master"):
            notify_merge_conflict(current_branch(), "master")
            return
    subprocess.run(["git", "push", "origin", "HEAD"])
    if mergeable_on_branch():
        print("merging " + current_branch() + " into " + branch_name())
        if subprocess.run(["git", "checkout", "master"]).returncode != 0:
            notify_failed_checkout("master")
            return
        if not merge(branch_name()):
            notify_merge_conflict(current_branch(), branch_name())


if __name__ == "__main__":
    main()
