---
title: Security
page: security
isSecurity: true
---

# Security

## Reporting security issues

The Haskell [**security advisory database**][advisory-db] documents
known issues in Haskell libraries and open source tools.  Anyone can
report **historical or low-impact issues** via the [public
submission process].

[advisory-db]: https://github.com/haskell/security-advisories
[public submission process]: https://github.com/haskell/security-advisories/blob/main/CONTRIBUTING.md

**High-impact vulnerabilities** should be reported privately to
[security-advisories@haskell.org](mailto:security-advisories@haskell.org)
(we do not use PGP).  Alternatively, high-impact vulnerabilities can
be reported via the CERT/CC [VINCE] system.  Use "Haskell
Programming Language" as the vendor name.

[VINCE]: https://kb.cert.org/vince/

The Security Response Team currently coordinates security response
under **embargo for high impact issues only**.  Factors that
influence whether or not we will deal with an issue under embargo
include:

- How severe is the vulnerability?
- How widely used is the library or tool in which the issue occurs?
- Does the issue also affect other ecosystems, or is there already a
  security response underway?  (We will not break someone else's
  embargo.)

For example, a high-severity vulnerability affecting the GHC
toolchain or a popular library would likely warrant an embargo.  If
you are unsure, please contact the Security Response Team and we
will help assess the impact.


## Haskell Security Response Team

The Haskell Security Response Team (SRT) coordinates security
response for high-impact vulnerabilities, and maintains the advisory
database and associated tooling.

The current members of the SRT are:

* **Fraser Tweedale**
* **Gautier Di Folco**
* **Lei Zhu**
* **Montez Fitzpatrick**
* **Mihai Maruseac**
* **Tristan de Cacqueray**

The SRT is an initiative of the [Haskell Foundation] pursuant to
[Tech Proposal #37][hf-tp-37].

[Haskell Foundation]: https://haskell.foundation/
[hf-tp-37]: https://github.com/haskellfoundation/tech-proposals/blob/main/proposals/accepted/037-advisory-db.md

## Security Guides

The SRT publishes security guides for Haskell programmers and
project maintainers.  Guides will be added or updated over time.

*  [How to secure GitHub repositories](https://github.com/haskell/security-advisories/blob/main/guides/github.md)

## SRT Reports

The SRT reports quarterly on our completed and ongoing work, and
future plans.

* [2024 Q4](https://github.com/haskell/security-advisories/blob/main/reports/2025-02-06-Q4-report.md)
* [2024 Q3](https://github.com/haskell/security-advisories/blob/main/reports/2024-11-14-Q3-report.md)
* [2024 Q2](https://github.com/haskell/security-advisories/blob/main/reports/2024-07-18-Q2-report.md)
* [2024 Q1](https://github.com/haskell/security-advisories/blob/main/reports/2024-04-08-Q1-report.md)
* [2023 Q3 & Q4](https://github.com/haskell/security-advisories/blob/main/reports/2024-01-10-half-year-report.md)
* [2023 Q2](https://github.com/haskell/security-advisories/blob/main/reports/2023-07-10-ann-q2-report.md)
