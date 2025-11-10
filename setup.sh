#!/usr/bin/env bash

# # use it with tmux and slime
# rm -r Music
# ./setup.sh
#
# # Checking the whole Music directory
# htagcli check directory ./Music
#
# # What if we move an album to another directory?
# mv Music/Artist_30/Album_1 Music/some-dir
# # htagcli detects some files are not at the right place
# htagcli check directory ./Music
#
# # Let's fix the path
# htagcli fix-paths directory ./Music
# # Now it should be fine again
# htagcli check directory ./Music
#
# # Ah! The cover file is still in the old directory. Let's move it back in the album directory.
# mv Music/some-dir/cover.jpg Music/Artist_30/Album_1/
# rmdir Music/some-dir
# # Everything is fine now
# htagcli check directory ./Music
#
# # htagcli can also make sure some tags are rightly set. Let's change some of them to see how this works.
# htagcli set --noyear --genre random files Music/Artist_20/Album_1/1-Track_1.mp3
# htagcli check directory ./Music
#
# # Let's fix that
# htagcli set --year 2025 --genre Pop/Rock files Music/Artist_20/Album_1/1-Track_1.mp3
# # All good again
# htagcli check directory ./Music

set -o nounset
set -o errexit
set -o pipefail

for i in $(seq 1 100); do
  for j in $(seq 1 10); do
    ARTIST="Artist_$i"
    ALBUM="Album_$j"
    mkdir -p "Music/$ARTIST/$ALBUM"
    touch "Music/$ARTIST/$ALBUM/cover.jpg"

    for k in $(seq 1 10); do
      TRACK_NUM=$k
      TRACK_TITLE="Track_$TRACK_NUM"
      FILE_PATH="Music/$ARTIST/$ALBUM/${TRACK_NUM}-${TRACK_TITLE}.mp3"

      echo "Creating $FILE_PATH"

      cp ~/development/htagcli/data/sample.mp3 "$FILE_PATH"
      htagcli set \
        --artist "$ARTIST" \
        --album "$ALBUM" \
        --title "$TRACK_TITLE" \
        --track "$TRACK_NUM" \
        --year "2025" \
        --genre "Pop/Rock" \
        files "$FILE_PATH"
    done
  done
done

