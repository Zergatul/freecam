package com.zergatul.freecam;

import net.minecraft.world.level.block.Block;

public interface ModLoaderBridge {
    WrappedRegistry<Block> getBlockRegistry();
}