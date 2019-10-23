---
id: read_files
title:  "File Channel"
---

A `AsynchronousFileChannel` provides API for handling files in non-blocking way.

Required imports for presented snippets:


## Basic operations 

Opening file for given path (with no additional open attributes) returns a `ZManaged` instance on which we're running the intended operations. `ZManaged` makes sure that the channel gets closed afterwards:

