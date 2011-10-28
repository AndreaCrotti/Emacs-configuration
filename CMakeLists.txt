#TODO: get the version from the git repository setting

cmake_minimum_required(VERSION 2.8)
#TODO: set NONE instead since we don't really build executables and 
#similar things but just run some shell commands
project(Emacs-Configuration NONE)

# check if I have updated something and recompile it in case is needed

# every single project has its own way to do things

macro (execute_in_subdir subdir command)
  execute_process(
    COMMAND ${command}
    WORKING_DIRECTORY ${subdir}
    )
endmacro ()

function (to_master_branch subdir)
  # do I ever need execute_process?
  execute_in_subdir(${subdir} "git checkout master")
endfunction()

set(simple_make make)
#TODO: add the setting for the Emacs output
set(autoconf "/usr/bin/autoreconf -fi && ./configure && make")

function (org_compile)
  execute_in_subdir(org_mode ${simple_make})
endfunction()

#TODO: we need to have all these tools
function (doxymacs)
  execute_in_subdir(doxymacs ${autoconf})
endfunction()

function (tramp)
  execute_in_subdir(tramp ${autoconf})
endfunction()

# these last two might be put together

add_custom_target(
  org-mode ALL
  cd org-mode && make
  # COMMAND org_compile()
  # # DEPENDS org_compile doxymacs tramp
  # COMMENT rebuild everything
)

add_custom_target(
  doxymacs ALL
  cd doxymacs && ${autoconf}
)

set(autoconf_projs tramp doxymacs)
set(makefile_projs org-mode cedet)

# foreach (aconf ${autoconf_projs})
  
#   #FIXME: this is also wrong because we need to define an output or a target
#   #TODO: take the name of the subproject automatically
#   add_custom_command(
#     # what if there is no actual output
#     # what if the target is the org-mode target previosly defined?
#     COMMAND autoreconf
#     ARGS -fi
#     COMMAND configure
#     COMMAND make
#     WORKING_DIRECTORY ${aconf}
#     )

# endforeach ()

# add_custom_target(
#   rebuild ALL
#   DEPENDS ${