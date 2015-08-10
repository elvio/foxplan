#!/bin/bash
erl +K true +A 128 -pa _build/default/lib/*/ebin -s foxplan
