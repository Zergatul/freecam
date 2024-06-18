# Freecam

### Build

#### Using Makefile
Check if you have `make` installed by running `make --version` in your Command Prompt/Terminal. If not, install it.

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

#### Using Gradlew
> [!NOTE] 
> Use the Gradlew.bat for Windows

To build mod by yourself go to Forge or Fabric directory and run `gradlew build`.

### Downloading
you can find the latest builds on the [GitHub Actions](https://github.com/Zergatul/freecam/actions) page.

Click on the latest build.

Then click on the artifact named `freecam-<modloader>-<version>.jar` to download it.
![Artifacts Tab](https://raw.githubusercontent.com/Zergatul/freecam/1.21/screenshots/Artifacts.png)