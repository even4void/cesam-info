#!/usr/bin/env bash
for i in $(ls figs/*); do pdfcrop --margins 5 $i $i; done
