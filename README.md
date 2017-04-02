# antikythera

CalDAV/CardDAV server in Haskell

## Basic Ideas

* We need full compliance with the RFCs for CalDAV and CardDav
(e.g.: [RFC4791][4791])
* web handling via Snap or Yesod -> For now I (nek0) am going with yesod,
because of familiarity.
* There are some libraries covering some aspects of the Task:
	* VCard: [vcard][vcard] or [hs-vcard][hs-vcard]
	* WebDAV: [DAV][DAV]
	* iCalendar: [iCalendar][iCalendar]

[4791]: https://tools.ietf.org/html/rfc4791.html
[vcard]: http://hackage.haskell.org/package/vcard
[hs-vcard]: http://hackage.haskell.org/package/hs-vcard
[DAV]: http://hackage.haskell.org/package/DAV
[iCalendar]: http://hackage.haskell.org/package/iCalendar
