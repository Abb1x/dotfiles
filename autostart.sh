

xrandr --output HDMI1 --primary --mode 1920x1080 --output LVDS1 --off && feh --bg-fill ~/Pictures/wallpapers/snow.jpg 

dte()
{
  dte="$(date +"%A, %B %d - %H:%M")"
  echo -e " $dte"
}
upd()
{
  upd="$(checkupdates | wc -l)"
  echo -e " $upd updates"
}
mem()
{
  mem=`free |awk '/Mem/ {printf "%d MiB/%d Mib\n", $3 / 1024.0, $2 / 1024.0}'`
  echo -e " $mem"
}
net()
{
  net="$(cat /sys/class/net/w*/operstate)"
  ip="$(ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1')"
  if [ "$net" != "up" ]; then
    echo -e "睊"
else
    echo -e "直 $ip"
fi
}
pkg()
{
  pkg="$(pacman -Q |wc -l)"
  echo -e " $pkg"
}
cpu()
{
 read cpu a b c previdle rest < /proc/stat
 prevtotal=$((a+b+c+previdle))
 sleep 0.5
 read cpu a b c idle rest < /proc/stat
 total=$((a+b+c+idle))
 cpu=$((100*( (total-prevtotal) - (idle-previdle) ) / (total-prevtotal) ))
 echo -e " $cpu% CPU"
}
while true; do
  xsetroot -name "$(cpu) | $(mem) |$(net) | $(upd) | $(pkg) | $(dte)"
  sleep 1m
done &
urxvtd -q &
#picom --backend glx --blur-background --blur-method dual_kawase
