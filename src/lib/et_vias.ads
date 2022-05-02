------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                VIAS                                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:


with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with cairo;

with et_string_processing;		use et_string_processing;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;
with et_terminals;				use et_terminals;
with et_drills;					use et_drills;
with et_pcb_stack;				use et_pcb_stack;
with et_design_rules;			use et_design_rules;
with et_text;					use et_text;

package et_vias is
	
	--use pac_geometry_brd;
	use pac_geometry_2;

	
	type type_micro_vias_allowed is (NO, YES);
	function to_micro_vias_allowed (allowed : in string) return type_micro_vias_allowed;
	function to_string (allowed : in type_micro_vias_allowed) return string;
	


	
	keyword_via_category 	: constant string := "category";

	-- for buried vias:
	keyword_layers			: constant string := "layers";

	-- for blind vias:
	keyword_destination		: constant string := "destination";

	-- For blind or buried vias use this type. This is about inner layers only.
	subtype type_via_layer is type_signal_layer range
		type_signal_layer'first + 1 -- the topmost inner layer
		.. 
		type_signal_layer'last - 1; -- the deepest inner layer.
		-- NOTE: The upper end of the range defined here does not
		-- suffice. The deepest inner layer of a particular buried or blind
		-- via must be validated against the pcb layer stack of the board.
		
	
	type type_buried_layers is record
		-- The topmost signal layer of the via:
		upper	: type_via_layer := type_via_layer'first;

		-- The deepest signal layer of the via:
		lower	: type_via_layer := type_via_layer'first;
		-- NOTE: It is reasonable to use as default the 
		-- same as for the upper layer. The last value in
		-- range type_via_layer would always produce a lower 
		-- layer much deeper than the deepest layer of the stack.
	end record;

	-- Converts two strings like "2" and "6" to a type_buried_layers.
	-- Checks the layers. The layers must be inner layers. Otherwise
	-- exception error is raised.
	function to_buried_layers (
		upper, lower	: in string; -- 2, 6
		bottom			: in type_signal_layer) -- 16
		return type_buried_layers;

	function to_string (layers : in type_buried_layers) return string;
	
	type type_via_category is (
		THROUGH,
		BURIED,
		BLIND_DRILLED_FROM_TOP,
		BLIND_DRILLED_FROM_BOTTOM
		);

	via_category_default : constant type_via_category := THROUGH;
	
	function to_string (category : in type_via_category) return string;
	function to_via_category (category : in string) return type_via_category;
	

	type type_via (category : type_via_category) is new type_drill with record

		-- Whatever the via category, there is always a restring 
		-- in inner layers (mostly wider than restring_outer).
		-- Exception: One or two layer boards do not have innner restring.
		restring_inner	: type_restring_width;
		
		case category is
			when THROUGH =>
				-- Restring in outer layers (top/bottom)
				restring_outer	: type_restring_width;

				-- CS: stop mask open top/bottom

			when BLIND_DRILLED_FROM_TOP =>
				restring_top	: type_restring_width;

				-- the deepest layer of the via:
				lower			: type_via_layer; -- CS rename to deepest
				
				-- CS: stop mask open
				
			when BLIND_DRILLED_FROM_BOTTOM =>
				restring_bottom	: type_restring_width;

				-- the topmost layer of the via:
				upper			: type_via_layer; -- CS rename to highest
				
				-- CS: stop mask open
				
			when BURIED =>
				layers : type_buried_layers;
				
		end case;
	end record;

	-- returns the properties of the given via as string:
	function to_string (via : in type_via) return string;


	-- Returns true if the given buried via uses the given layer.
	-- The given via must be of category BURIED. Otherwise an exception
	-- will be raised.
	-- NOTE: If The given layer is the top layer (1) then the
	-- return is false, because a buried via never uses this layer.
	function buried_via_uses_layer (
		via		: in type_via;
		layer	: in type_signal_layer)
		return boolean;

	-- Returns true if the given blind via uses the given layer.
	-- The given via must be of category BLIND_DRILLED_FROM_TOP or
	-- BLIND_DRILLED_FROM_BOTTOM. Otherwise an exception will be raised.
	-- The bottom layer must be provided if the via is a
	-- BLIND_DRILLED_FROM_BOTTOM.
	function blind_via_uses_layer (
		via		: in type_via;
		layer	: in type_signal_layer;
		bottom	: in type_signal_layer := type_signal_layer'last)
		return boolean;

	
	-- vias are collected in simple lists
	package pac_vias is new indefinite_doubly_linked_lists (type_via);


	-- Iterates the vias. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		vias	: in pac_vias.list;
		process	: not null access procedure (position : in pac_vias.cursor);
		proceed	: not null access boolean);

	

	via_text_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	-- This constant defines the text size of the layer numbers
	-- that are displayed in the center of the via.
	-- The final text size is calculated by this formula:
	-- size = drill radius * text_size_multiplier.
	-- CS: Adjustment required for layer numbers greater 10.
	text_size_factor : constant type_distance_positive := 0.3;

	-- Above the net name the layer numbers are displayed.
	-- Below the net name the drill size is displayed.
	-- This constant defines the position of layer numbers
	-- and drill size.
	text_position_layer_and_drill_factor : constant type_distance_positive := 0.4;

	
	type type_user_specific_drill_size is record
		active	: boolean := false;
		size	: type_drill_size := type_drill_size'first;
	end record;
	
	type type_user_specific_restring is record
		active	: boolean := false;
		width	: type_restring_width := type_restring_width'first;
	end record;



	type type_user_settings_vias is record
		drill			: type_user_specific_drill_size;
		restring_inner	: type_user_specific_restring;
		restring_outer	: type_user_specific_restring;
	end record;

	
end et_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
