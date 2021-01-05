------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                                VIAS                                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with et_string_processing;		use et_string_processing;

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;
with et_terminals;				use et_terminals;
with et_drills;					use et_drills;
with et_pcb_stack;				use et_pcb_stack;
with et_design_rules;			use et_design_rules;


package et_vias is
	
	use et_pcb_coordinates.pac_geometry_brd;

	
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
		lower	: type_via_layer := type_via_layer'last;
	end record;

	-- Converts a string like "2-6" to a type_buried_layers.
	function to_buried_layers (text : in string) return type_buried_layers;

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
				lower			: type_via_layer;
				-- CS: stop mask open
				
			when BLIND_DRILLED_FROM_BOTTOM =>
				restring_bottom	: type_restring_width;
				upper			: type_via_layer;
				-- CS: stop mask open
				
			when BURIED =>
				layers : type_buried_layers;
				
		end case;
	end record;

	-- vias are collected in simple lists
	package pac_vias is new indefinite_doubly_linked_lists (type_via);

	
end et_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
