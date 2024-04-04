### Checklist

Please:

- [ ] Make sure this PR is against "dev", not "main".
- [ ] Request a review from one of the current epipredict main reviewers:
      dajmcdon.
- [ ] Make sure to bump the version number in `DESCRIPTION` and `NEWS.md`.
      Always increment the patch version number (the third number), unless you are
      making a release PR from dev to main, in which case increment the minor
      version number (the second number).
- [ ] Describe changes made in NEWS.md, making sure breaking changes
      (backwards-incompatible changes to the documented interface) are noted.
      Collect the changes under the next release number (e.g. if you are on
      0.7.2, then write your changes under the 0.8 heading).
- [ ] Consider pinning the `epiprocess` version in the `DESCRIPTION` file if
  - You anticipate breaking changes in `epiprocess` soon
  - You want to co-develop features in `epipredict` and `epiprocess`

### Change explanations for reviewer

### Magic GitHub syntax to mark associated Issue(s) as resolved when this is merged into the default branch

- Resolves #{issue number}
