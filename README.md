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

If you want to build universal jar, go to `universal` folder, and run `gradlew universalJar` in the same way as above. By default it builds all loader jars. Use `-Ploaders=fabric,neoforge` to choose which loader jars to include.

### Downloading
You can find the latest builds on the [GitHub Actions](https://github.com/Zergatul/freecam/actions) page. Click on the latest build. At the right bottom there should be **Artifacts** section, from where you can download jar.