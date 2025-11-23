buildscript {
    repositories {
        maven("https://repo.spongepowered.org/maven")
        mavenCentral()
    }
}

plugins {
    java
}

java.toolchain.languageVersion = JavaLanguageVersion.of(21)

repositories {
    maven("https://repo.spongepowered.org/maven")
    mavenCentral()
}

dependencies {
    implementation("org.ow2.asm:asm:9.6")
    implementation("org.ow2.asm:asm-tree:9.6")
    implementation("org.spongepowered:mixin:0.8.7")
}

tasks.withType<JavaCompile> {
    sourceCompatibility = "21"
    targetCompatibility = "21"
}

sourceSets.main {
    java {
        srcDir("../common/mixin-plugin/java")
    }
}