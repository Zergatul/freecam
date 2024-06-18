# Freecam

### Build

#### Using Makefile
Check if you have `make` installed by running `make --version`. If not, install it.
to Build the Mod execute the Command for your specific Modloader.
The jar will be located in `<modloader>/build/libs/`.

for Fabric:
```shell
make build-fabric
```

for Forge:
```shell
make build-forge
```

for Both
```shell
make all
```

#### Manuell
> [!NOTE] 
> You may consider using the Makefile

To build mod by yourself go to Forge or Fabric directory and run `gradlew build`.