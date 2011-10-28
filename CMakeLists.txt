#TODO: get the version from the git repository setting

cmake_minimum_required(VERSION 2.8)
#TODO: set NONE instead since we don't really build executables and 
#similar things but just run some shell commands
project(Emacs-Configuration C)

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
set(autoconf "autoreconf -fi && ./configure && make")

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
  rebuild
  COMMAND org_compile()
  # DEPENDS org_compile doxymacs tramp
  COMMENT rebuild everything
)
