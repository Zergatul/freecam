package com.zergatul.freecam;

import com.zergatul.freecam.mixins.DebugScreenEntriesAccessor;
import net.minecraft.client.gui.components.debug.DebugScreenDisplayer;
import net.minecraft.client.gui.components.debug.DebugScreenEntry;
import net.minecraft.resources.Identifier;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.chunk.LevelChunk;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public class DebugScreenExtensions {

    private static final Identifier POSITION = Identifier.fromNamespaceAndPath("freecam", "position");
    private static final Identifier LOOKING_AT_BLOCK = Identifier.fromNamespaceAndPath("freecam", "looking_at_block");

    public static void register() {
        DebugScreenEntriesAccessor.register_FC(POSITION, new FreeCamPositionDebugScreenEntry());
        DebugScreenEntriesAccessor.register_FC(LOOKING_AT_BLOCK, new FreeCamLookingAtBlockDebugScreenEntry());
    }

    private static class FreeCamPositionDebugScreenEntry implements DebugScreenEntry {
        @Override
        public void display(DebugScreenDisplayer displayer, @Nullable Level level, @Nullable LevelChunk chunk1, @Nullable LevelChunk chunk2) {
            FreeCam.INSTANCE.onShowDebugScreenCoordinates(POSITION, displayer);
        }
    }

    private static class FreeCamLookingAtBlockDebugScreenEntry implements DebugScreenEntry {
        @Override
        public void display(DebugScreenDisplayer displayer, @Nullable Level level, @Nullable LevelChunk chunk1, @Nullable LevelChunk chunk2) {
            FreeCam.INSTANCE.onShowLookingAtBlock(LOOKING_AT_BLOCK, displayer);
        }
    }
}