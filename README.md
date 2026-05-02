# FreeCam

### Build
To build the mod you need to have JDK 25 installed.

If you need version for single modloader, go to its directory and run:
```shell
# Windows Command Line
gradlew build

# Linux, Mac OS, PowerShell
./gradlew build
```

The mod jar will be created in the `<modloader>/build/libs` folder.

If you want to build universal jar, to go `universal` folder, and run gradlew build in the same way as above.

### Downloading
You can find the latest builds on the [GitHub Actions](https://github.com/Zergatul/freecam/actions) page. Click on the latest build. At the right bottom there should be **Artifacts** section, from where you can download jar.