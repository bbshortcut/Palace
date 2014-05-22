# Palace

*Palace* is a set of integrated tools for personal information
management.

## Available Tools

*Palace* currently provides the following tools:

### cards

*cards* allows to manage a set of flash cards and learn them with
[spaced repetition technique](http://en.wikipedia.org/wiki/Spaced_repetition).

*cards* provides the following commands:
- cards add: add a flash card to the deck
- cards edit [ids...]: edit the last/some flash cards of the deck
- cards export: export all the flash cards of the deck
- cards info: say how many flash cards there are in the deck and how
  many are due
- cards list: list all the flash cards of the deck
- cards pick [ids...]: pick the first due/some flash cards from the
  deck
- cards remove [ids...]: remove the last/some flash cards from the
  deck

### volumes

*volumes* allows to backup/restore volumes, associated with machines,
on removeable drives.  Each volume is a file forest that may or may
not be available at a given volume point on a given machine.  If it is
then a backup point is also defined and hosts timestamped backups.
*volumes* builds backups with hard links so that only data changed
since previous backups is transferred (incremental backups).

*volumes* provides the following commands:
- volumes backup [volumes...]: backups volumes
- volumes restore [volumes...]: restores volumes

## Expected Tools

*Palace* will also include the following tools:
- contacts,
- contexts,
- dates,
- notes,
- tasks,
- transactions.
