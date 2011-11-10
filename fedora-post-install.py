
#!/bin/python
import subprocess
import logging
import shutil
import os
import urllib
import re
import stat


class globals:
    is64BitInstall = False
    pattern32bit = re.compile('i.86')
    pattern64bit = re.compile('_64')

packagesSystem = ['gpointing-device-settings', 'dconf-editor', 'numlockx',
                  'nautilus-image-converter', 'nfs-utils',
                  'system-config-services', 'alacarte', 'yum-plugin-fastestmirror',
                  'htop', 'trash-cli', 'dkms', 'gksudo']

packagesTools = ['ncftp', 'lynx', 'elinks', 'rtorrent', 'irssi', 'tor', 'privoxy',
                 'mutt', 'slrn', 'screen', 'nano', 'vim', 'parcellite', 'conky',
                 'gparted', 'zsh', 'units', 'p7zip', 'p7zip-plugins', 'finger',
                 'gpm', 'man-pages', 'linuxdoc-tools', 'livecd-tools', 'evince',
                 'gnochm', 'archmage', 'filezilla', 'telnet']

packagesOffice = ['libreoffice-impress', 'libreoffice-writer', 'libreoffice-calc',
                  'libreoffice-langpack-de', 'aspell-de', 'aspell-en', 'planner',
                  'antiword', 'docbook2X', 'calibre', 'pdftk', 'diffpdf', 'psutils',
                  'pdfjam', 'pdf2djvu', 'evince-djvu', 'ImageMagick-djvu', 'djview4',
                  'AdobeReader_deu']

packagesDesign = ['ImageMagick', 'feh', 'xsane', 'gimp', 'gimp-help', 'gimp-data-extras',
                  'inkscape', 'dia', 'scribus', 'gtk-recordmydesktop']

packagesDevGeneric = ['mercurial', 'bzr', 'rcs', 'gcc', 'git', 'cvs', 'subversion',
                      'rpmdevtools', 'glade3', 'dwdiff', 'doxygen', 'diffutils', 'meld',
                      'redet', 'autoconf', 'automake']

packagesDevNetbeans = ['http://bits.netbeans.org/7.0.1/community/latest/bundles/netbeans-7.0.1-ml-linux.sh']

packagesDevPython = ['xmlto', 'epydoc', 'perl-XML-Twig', 'python-docs' 'python-docutils',
                     'ipython']

packagesEmacs = ['emacs', 'emacs-bbdb', 'ctags', 'ctags-etags', 'psgml', 'w3m-el', 'emacs-a2ps',
                 'emacs-magit', 'emacs-mercurial', 'emacs-goodies', 'emacs-color-theme']

packagesMedia = ['gstreamer-plugins-ugly', 'gstreamer-plugins-bad', 'gstreamer-ffmpeg', 'lame', 'ffmpeg', 'python-eyed3',
                 'mplayer', 'mencoder', 'dvdrip', 'libdvdcss', 'audacity', 'avidemux', 'gtkpod', 'mpd', 'ncmpcpp',
                 'gpodder', 'abcde', 'mpg321', 'id3v2', 'mp3gain', 'picard', 'gnome-subtitles', 'soundconverter',
                 'streamripper', 'mkvtoolnix-gui', 'mkvtoolnix', 'ogmrip', 'vlc', 'sox', 'faac', 'x264', 'vobcopy', 'flash-plugin']

packagesDevJava = ['java-1.6.0-openjdk-devel', 'java-1.6.0-openjdk-javadoc', 'icedtea-web', 'ant', 'junit']

packagesDevWeb = ['httpd', 'php', 'php-devel', 'php-gd', 'php-imap', 'php-ldap', 'php-mysql', 'php-odbc', 'php-pear',
                'php-xml', 'php-xmlrpc', 'php-eaccelerator', 'php-magickwand', 'php-mapserver',
                'php-mbstring', 'php-mcrypt', 'php-mhash', 'php-mssql', 'php-shout', 'php-snmp', 'php-soap', 'php-tidy',
                'curl', 'curl-devel', 'perl-libwww-perl', 'libxml2', 'libxml2-devel', 'mysql', 'mysql-devel', 'mysql-server',
                'httpd-devel', 'mod_python', 'mod_perl', 'perl-HTML-Parser', 'perl-DBI', 'perl-Net-DNS', 'perl-Digest-SHA1',
                'perl-ExtUtils-AutoInstall', 'perl-NetAddr-IP', 'perl-Archive-Tar', 'phpMyAdmin', 'mysql-workbench',
                'php-pecl-xdebug', 'php-pear-PhpDocumentor', 'php-phpunit-PHPUnit', 'php-pear-PHP-CodeSniffer' ]

packagesCompat = ['wine', 'samba-client']

packagesFonts = ['google-droid-sans-fonts', 'google-droid-sans-mono-fonts', 'google-droid-serif-fonts', 
                 'bitstream-vera-sans-fonts', 'bitstream-vera-sans-mono-fonts', 'bitstream-vera-serif-fonts']

packagesTex = ['texlive', 'texlive-latex', 'texlive-xetex', 'texlive-utils', 'texlive-doc', 'texlive-dvips',
                'texlive-dviutils', 'latexmk', 'chktex', 'latexdiff']

packagesMath = ['qtoctave', 'octave', 'octave-doc', 'gnuplot', 'gnuplot-doc', 'gnuplot-latex',
                'emacs-gnuplot', 'maxima', 'maxima-gui', 'asymptote']

packagesEyecandy = ['faenza-icon-theme']

packagesVirtualBox = ['http://download.virtualbox.org/virtualbox/4.1.6/VirtualBox-4.1-4.1.6_74713_fedora16-1.x86_64.rpm',
                      'http://download.virtualbox.org/virtualbox/4.1.6/VirtualBox-4.1-4.1.6_74713_fedora16-1.i686.rpm']

configShell = ['.zsh_history', '.zshrc', '.mutt', '.muttrc', '.mailcap', '.netrc', '.hgrc', '.taskrc', '.vimrc', '.slrnrc', '.irssi', '.abcde.conf']

configEmacs = ['.emacs.d', '.emacs', '.emacs-w3m', '.emacs-places', '.emacs-custom.el']

configMozilla = ['.mozilla']

configFonts = ['.fonts']


def taskAddRepos():
    logging.info('  taskAddRepos: Adding some repos.')
    #subprocess.check_call('rpm -Uvh "http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-stable.noarch.rpm" "http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-stable.noarch.rpm"', shell=True)
    subprocess.check_call('rpm -ivh http://rpm.livna.org/livna-release.rpm', shell=True) 
    
    if self.is64BitInstall:
        subprocess.check_call('rpm -ivh http://linuxdownload.adobe.com/adobe-release/adobe-release-x86_64-1.0-1.noarch.rpm', shell=True)
    else:
        subprocess.check_call('rpm -ivh http://linuxdownload.adobe.com/adobe-release/adobe-release-i386-1.0-1.noarch.rpm', shell=True)
    subprocess.check_call('rpm --import /etc/pki/rpm-gpg/RPM-GPG-KEY-adobe-linux', shell=True)
    subprocess.check_call('yum update -y')

def taskInstallExternalPackages(mode, urlBundle):
    logging.info('  taskInstallExternalPackages: Fetching files.')
    if mode == 'rpm':
        for url in urlBundle:
            if globals.is64BitInstall == True and globals.pattern64bit.search(url):
                installCommand = 'rpm -ivh ' + url
                subprocess.check_call(installCommand, shell=True)
            elif globals.is64BitInstall == False and globals.pattern32bit.search(url):
                installCommand = 'rpm -ivh ' + url
                subprocess.check_call(installCommand, shell=True)
    if mode == 'sh':
        for url in urlBundle:
            f, header = urllib.urlretrieve(url)
            installCommand = 'sh < ' + f 
            subprocess.check_call(installCommand, shell=True)

def taskUpdateCodecs():
    logging.info('  taskUpdateCodecs: Downloading codecs.')
    codecsUrl = 'http://www.mplayerhq.hu/MPlayer/releases/codecs/all-20110131.tar.bz2'
    codecsFile, codecsFileHeader = urllib.urlretrieve(codecsUrl)
    targetDir = '/usr/lib/codecs'
    logging.info('  taskUpdateCodecs: Installing codecs.')
    installCommand = 'tar xvf ' + codecsFile + ' --strip-components 1 -C ' + targetDir
    subprocess.check_call(installCommand, shell=True)
    
def taskInstallPackages(packageBundle):
    logging.info('  taskInstallPackages: Installing packages.')
    packageList = ' '.join(packageBundle)
    installCommand = 'yum install ' + packageList + ' -y'
    subprocess.check_call(installCommand, shell=True)

def taskRestoreConfig(configBundle):
    configList = '\n\t'.join(configBundle)
    logText = '  taskRestoreConfig: Restoring configuration files from: \n\t' + configList
    logging.info(logText)
    for configObj in configBundle:
	src = os.path.join('/home/chm-old', configObj)
        dest = os.path.join('/home/chm', configObj)
	shutil.move(src, dest)

def taskRestoreEyecandy():
    subprocess.check_call('su chm -c "gsettings set org.gnome.desktop.interface icon-theme Faenza"')

def taskTrackpointSpeed():
    logging.info('  taskTrackpointSpeed: Speeding up trackpoint.')
    #udevRulesFilename = '/etc/udev/rules.d/10-trackpoint.rules'
    udevRulesFilename = '/home/chm/10-trackpoint.rules'
    fh = open(udevRulesFilename,"w")
    fh.write('SUBSYSTEM=="serio", DRIVERS=="psmouse", ATTR{speed}="220", ATTR{sensitivity}="190"\n')
    fh.close
    subprocess.check_call('/sbin/udevadm trigger', shell=True)

def main():
    #taskInstallPackages(packagesSystem)
    #taskInstallPackages(packagesTools)
    #taskInstallPackages(packagesOffice)
    #taskInstallPackages(packagesDesign)
    #taskInstallPackages(packagesDevGeneric)
    #taskInstallPackages(packagesDevPython)
    #taskInstallPackages(packagesDevJava)
    #taskInstallPackages(packagesDevWeb)
    #taskInstallExternalPackages('sh', packagesDevNetbeans)
    #taskInstallPackages(packagesCompat)
    #taskInstallPackages(packagesEmacs)
    #taskRestoreConfig(configEmacs)
    #taskUpdateCodecs()
    #taskTrackpointSpeed()
    #taskAddRepos()
    #taskInstallPackages(packagesMedia)
    #taskInstallPackages(packagesMath)
    #taskInstallPackages(packagesTex)
    #taskInstallPackages(packagesFonts)
    #taskInstallPackages(packagesEyecandy)
    #taskRestoreEyecandy()
    taskInstallExternalPackages('rpm', packagesVirtualBox)

if __name__ == "__main__":
    logging.basicConfig(format='%(asctime)s %(message)s', datefmt='%Y-%m-%d %H:%M:%S', level=logging.INFO)
    main()
