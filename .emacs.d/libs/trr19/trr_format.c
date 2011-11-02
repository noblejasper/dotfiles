/* format.c -- a text formatter for TRR19
 * Last modified on Sun Jun 30 02:59:53 1996 
 * Copyright (C) 1996 Yamamoto Hirotaka <ymmt@is.s.u-tokyo.ac.jp>
 */

/* This file is a part of TRR19, a type training software on
 * GNU Emacs.
 *
 * You can redistribute it and/or modify it under the terms of
 * the version 2 of GNU General Public License as published by the
 * Free Software Foundation.
 *
 * TRR19 is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>

#if defined(HAVE_STRING_H)
#include <string.h>
#else
#include <strings.h>
#endif /* HAVE_STRING_H */  

#if defined(HAVE_FCNTL_H)
#include <fcntl.h>
#else
#if defined(HAVE_SYS_FCNTL_H)
#include <sys/fcntl.h>
#else
#include <sys/file.h>
#endif /* HAVE_SYS_FCNTL_H */
#endif /* HAVE_FCNTL_H */


main(int argc, char **argv){
  char textfile[256], formattedfile[256], lockfile[256], *tmpfname;
  char command[256], line[1024];
  FILE *fd, *tmpfd;
  int i;

  /* ignore signals */
  signal(SIGHUP, SIG_IGN);
  signal(SIGINT, SIG_IGN);
  signal(SIGQUIT, SIG_IGN);
  signal(SIGTERM, SIG_IGN);

  strcpy(textfile, TEXT_DIR);
  strcat(textfile, argv[1]);
  strcpy(formattedfile, textfile);
  strcat(formattedfile, ".formed");
  strcpy(lockfile, textfile);
  strcat(lockfile, ".lock");

  umask(18);

  /* if previous process is formatting same target text,
     wait for that process to finish formatting. */
  if (open(lockfile, O_CREAT|O_EXCL, 420) == -1)
    if (errno == EEXIST){
      i = 0;
      while (open(lockfile, O_CREAT|O_EXCL, 420) == -1){
	if (errno == EEXIST){
	  sleep(1);
	  /* if failed 20 times, then remove lockfile and exit abnormally */
	  if (i++ == 20){
	    unlink(lockfile);
	    exit(1);
	  }
	} else{
	  perror(lockfile);
	  exit(1);
	}
      }
      /* successfully formatted */
      unlink(lockfile);
      return 0;
    } else{
      perror(lockfile);
      exit(1);
    }
  else{
    /* format a text */
    tmpfname = tmpnam(NULL);
    unlink(formattedfile);
    sprintf(command, "%s -v '^[ \t]*$' %s | %s 's/\\([.?!;]\\) *$/\\1/' | %s 's/^  *\\(.*\\)$/\\1/' > %s",
	    GREP, textfile, SED, SED, tmpfname);
    system(command);

    tmpfd = fopen(tmpfname, "r");
    fd = fopen(formattedfile, "w");

    while(fgets(line, 1024, tmpfd))
      fputs(line, fd);

    fclose(tmpfd);
    fclose(fd);
    unlink(tmpfname);

    /* release lock */
    unlink(lockfile);
    return 0;
  }
}
