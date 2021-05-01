# dsBoltzmannMachinesClient

This is the client side R package for using Deep Boltzmann machines as generative models in [DataSHIELD](https://github.com/datashield). It depends on the [BoltzmannMachineRPlots](https://github.com/stefan-m-lenz/BoltzmannMachinesRPlots) package and on the basic DataSHIELD client package.

The corresponding server side R package is [dsBoltzmannMachines](https://github.com/stefan-m-lenz/dsBoltzmannMachines).

You can find a first conference abstract about this work here:
https://dx.doi.org/10.3205/19gmds062

There will be a full article available soon.

## Installation

### Client side
Execute the following R commands to install the client side package via the `devtools` package:

```R
devtools::install_github("stefan-m-lenz/BoltzmannMachinesRPlots", ref = "v0.1.0")
devtools::install_github("stefan-m-lenz/dsBoltzmannMachinesClient", ref = "v1.0.0")
```

### Server side

On the [Opal server](http://opaldoc.obiba.org/en/latest/admin/installation.html), the [dsBoltzmannMachines](https://github.com/stefan-m-lenz/dsBoltzmannMachines) must be installed on the R server of Opal.
This requires that Julia and the [BoltzmannMachines](https://github.com/stefan-m-lenz/BoltzmannMachines.jl) Julia package is also installed.

The following bash script, run with root provileges, can be used to set this up:

```bash
R -e 'install.packages("JuliaConnectoR")'

wget https://github.com/stefan-m-lenz/dsBoltzmannMachines/releases/download/v1.0.1/dsBoltzmannMachines_1.0.2.tar.gz
R -e 'install.packages("dsBoltzmannMachines_1.0.2.tar.gz", repos = NULL, type = "source")'
rm dsBoltzmannMachines_1.0.2.tar.gz


JULIA_VERSION=julia-1.0.5
wget https://julialang-s3.julialang.org/bin/linux/x64/1.0/$JULIA_VERSION-linux-x86_64.tar.gz
tar -xzf $JULIA_VERSION-linux-x86_64.tar.gz
rm $JULIA_VERSION-linux-x86_64.tar.gz
mkdir /opt/julia
mv $JULIA_VERSION /opt/julia/$JULIA_VERSION/
ln -s /opt/julia/$JULIA_VERSION/bin/julia /usr/local/bin/julia

# Set JULIA_BINDIR environment variable for the rserver
# and install the BoltzmannMachines Julia package as user "rserver".
BOLTZMANN_UPDATE_CMD=$(cat << 'END'
julia -e 'using Pkg; Pkg.add(PackageSpec(name = "BoltzmannMachines", version = "1.2"))'
END
)
su -s "/bin/bash" rserver -c "$BOLTZMANN_UPDATE_CMD"
JULIA_BINDIR="/opt/julia/$JULIA_VERSION/bin/"
echo "JULIA_BINDIR=$JULIA_BINDIR" >> /etc/environment
printf "\nSys.setenv(JULIA_BINDIR=\"$JULIA_BINDIR\")\n\n" >> /var/lib/rserver/conf/Rprofile.R
```

For the environment variabes set in the R server to take effect, the R server needs
to be restarted. If Opal has been installed via the RPM package, this can be done via `service rserver restart`.
