extra_kwargs = {}
try:
    from setuptools import setup
    extra_kwargs['install_requires'] = ['rope >= 0.9.3', 'ropemode']
except ImportError:
    from distutils.core import setup


classifiers=[
    'Development Status :: 4 - Beta',
    'Operating System :: OS Independent',
    'Environment :: X11 Applications',
    'Environment :: Win32 (MS Windows)',
    'Environment :: MacOS X',
    'Intended Audience :: Developers',
    'License :: OSI Approved :: GNU General Public License (GPL)',
    'Natural Language :: English',
    'Programming Language :: Python',
    'Topic :: Text Editors :: Emacs',
    'Topic :: Software Development']

def get_long_description():
    lines = open('README.txt').read().splitlines(False)
    end = lines.index('Setting Up')
    return '\n' + '\n'.join(lines[:end]) + '\n'

setup(name='ropemacs',
      version='0.6',
      description='An emacs mode for using rope python refactoring library',
      long_description=get_long_description(),
      packages=['ropemacs'],
      author='Ali Gholami Rudi',
      author_email='aligrudi@users.sourceforge.net',
      url='http://rope.sf.net/ropemacs.html',
      license='GNU GPL',
      classifiers=classifiers,
      requires=['ropemode'],
      **extra_kwargs)
