#!/bin/bash
leo-dev --startup-reference-time-ms $(perl -MTime::HiRes=time -e 'print int(time() * 1000)')
