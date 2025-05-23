------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                VIAS                                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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


with ada.containers; 			use ada.containers;
with ada.containers.indefinite_doubly_linked_lists;

with et_board_coordinates;		use et_board_coordinates;
with et_contour_to_polygon;
with et_drills;					use et_drills;
with et_pcb_stack;				use et_pcb_stack;
with et_design_rules_board;		use et_design_rules_board;
with et_fonts;					use et_fonts;


package et_vias is
	
	use pac_polygons;
	use pac_geometry_2;

	
	type type_micro_vias_allowed is (NO, YES);
	function to_micro_vias_allowed (allowed : in string) return type_micro_vias_allowed;
	function to_string (allowed : in type_micro_vias_allowed) return string;
	
	

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



	-- Computes the bounding-box of a via:
	function get_bounding_box (
		via : in type_via)
		return type_area;


	

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
	use pac_vias;


	-- Returns the position of a via as a string:
	function to_string (
		via : in pac_vias.cursor) 
		return string;


	function get_position (
		via : in pac_vias.cursor)
		return type_vector_model;
	

	function is_selected (
		via : in pac_vias.cursor)
		return boolean;

	
	function is_proposed (
		via : in pac_vias.cursor)
		return boolean;


	function is_moving (
		via : in pac_vias.cursor)
		return boolean;

	
	
	-- Iterates the vias. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		vias	: in pac_vias.list;
		process	: not null access procedure (position : in pac_vias.cursor);
		proceed	: not null access boolean);




	via_text_font : constant type_font :=
		to_font (FAMILY_MONOSPACE, SLANT_NORMAL, WEIGHT_NORMAL);

	
	-- This constant defines the text size of the layer numbers
	-- and net names
	-- that are displayed in the center of the via.
	-- The final text size is calculated by this formula:
	-- size = drill diameter * ratio_diameter_to_text_size.
	-- CS: Adjustment required for layer numbers greater 10.
	ratio_diameter_to_text_size : constant type_distance_positive := 0.2;


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



	-- Builds a polygon from the given via position, 
	-- restring and diameter:
	function to_polygon (
		position	: in type_vector_model;
		restring	: in type_restring_width;
		diameter	: in type_drill_size;
		tolerance	: in type_distance_positive)
		return type_polygon;
	
		
end et_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
