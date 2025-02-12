# R package cxlib

cxlib is a collection of R utilities, functions and object for library management, setup, configuration and interacting with data and analysis in Life Science GxP and regulated environments.

This is an initial development release to explore the functionality across several different platforms and integrated within compute environments.

This release of the cxlib packages currently only supports executing programs under audited conditions using `cxlib_batch()`. Future version will incorporate R package library management, support for {renv}, regulatory compliant audit trail, using remote services and much more.

<br/>



## Getting Started

Download and install the latest release of cxlib from https://github.com/mmengelbier/dev-r-package-cxlib/releases/latest

You can also install the latest release directly using `install.packages()`.   

```
install.packages( "https://github.com/mmengelbier/dev-r-package-cxlib/releases/download/v0.0.1/cxlib_0.0.1.tar.gz", type = "source", INSTALL_opts = "--install-tests" )
```

To install prior releases, replace the version number references in the URL.

<br/>


### Running Programs in Batch 

The function `cxlib_batch()` allows you to run a sequence of programs in batch mode in the specified order. 

```
cxlib::cxlib_batch( c( "path/to/my/first_program.R", "path/to/my/second_program.R" ) )
```

<br/>

Each program is run independently generating one log per program. By default, the log file name retains the program name with the `.Rout` file extension and is saved 
in the program directory. Options `logs`and `log.fileext` will allow logs to be stored in a different directory and with a different file extension.

```
cxlib::cxlib_batch( c( "path/to/my/first_program.R", "path/to/my/second_program.R" ), 
                    options = list( "logs" = "path/to/my/logs", "log.fileext" = "log" ) )
```

<br/>

### Paths and the Work Area
The cxlib package was developed for use with repositories,  containers and traditional R IDE's, meaning that all paths are relative to the directory structure that is consistent across development and analysis environments, containers, etc. A good practice is to set the R session working directory to the root of the repository as that should be consistent regardless of environment and where the repository is cloned.

A simple `cxlib_batch()` concept is that all paths are _relative_ to the current working directory, representing the repository root. 

All programs are also executed in a temporary directory structure, the _work area_ (that is how we can audit the program execution without extra software and licenses or impacting IT operations). The directory structure required for the submitted programs are recreated in the work area, including staging program files, all specified inputs and output directory structures (see Annotations).

_The default R session working directory for submitted programs is set to the work area directory_.


<br/>


### Annotations
The cxlib package features use annotations within programs to provide the necessary controls instead of external control lists or configuration files. 

An annotation is a special comment using the standard format `# @keyword value`,

The annotation can be defined anywhere in a program, allowing you to annotate where the input is used and where the output is created. 

One or more spaces delimit the annotation keyword and value. Leading and trailing spaces of the value are trimmed. Any white space before the `#` symbol and between the `#`and `@` symbols are disregarded to allow indentation aligned with code and other comments.

The following annotations are supported.

+ `cx.input` - Input files
+ `cx.output` - Output directories

Annotations not supported are ignored. 

<br/>


#### @cx.input
All program inputs are defined using the `@cx.input` annotation.

`@cx.input file` would stage the specified file in the work area.

`@cx.input directory` would stage all files in the specified directory in the work area. The directive is not recursive.

If the specified `file` or `directory` does not exist, the parent directory structure will not be staged in the work area and will likely result in an
R error or warning.

To delete a file, it must exist as an input and its parent directory must be specified as an output.

<br/>


#### @cx.output
The `@cx.output` annotation defines specific directories that the execution process will monitor for created, updated and deleted files.

`@cx.output directory` would stage the specified directory in the work area. The directive is not recursive.

If the specified output directory does not exist in the current working directory, an error is generated.

Any file created, updated or deleted in directories not specified as an output directory will be ignored.

Note, the program log file is always included as an output and does not require an output annotation.

<br/>



### Auditing Method
The audit method is based on simple directory inventories that are performed before and after a program executes. The independent work area is used so that the inventory does not have to traverse the entire compute environment's directory structure. 

_Created files_ are those files that exist after the program executes but did not exist in the inventory before.

_Updated files_ exist in both the inventories before and after the program executes. The inventory computes the SHA-1 digest/hash for each file in the inventory, so if the SHA-1 digests are different before and after the program executes, the file must have been updated.

_Deleted files_ are those files that existed before the program executed but did not exist in the inventory afterwards.

The list of inputs available when a program executes is readily available as the audit method is used for each individual program. That would permit that _input files_ can also be included in the audit, and more specifically which version of each input file since the SHA-1 digest is derived as part of the inventories.
