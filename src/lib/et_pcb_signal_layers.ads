------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          PCB / SIGNAL LAYERS                             --
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



with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.vectors;
with ada.containers.ordered_sets;

with et_board_geometry;			use et_board_geometry;
with et_string_processing; 		use et_string_processing;
with et_logging;				use et_logging;



package et_pcb_signal_layers is
	

	
	type type_signal_layer_category is (OUTER_TOP, INNER, OUTER_BOTTOM); 
	-- CS does the order matter ? Use this instead:
	--type type_signal_layer_category is (OUTER_TOP, OUTER_BOTTOM, INNER); 

	--CS subtype type_signal_layer_category_outer is type_signal_layer_category
		--range (OUTER_BOTTOM .. OUTER_BOTTOM);


	function to_string (
		category	: in type_signal_layer_category)
		return string;

	
	function invert_category (cat : in type_signal_layer_category)
		return type_signal_layer_category;
	
	
	signal_layer_top : constant positive := 1; -- CS rename signal to conductor
	signal_layer_bottom : constant positive := 100;
	type type_signal_layer is range signal_layer_top .. signal_layer_bottom;

	signal_layer_default : constant type_signal_layer := type_signal_layer'first;

	function to_string (layer : in type_signal_layer) return string;
	function to_signal_layer (layer : in string) return type_signal_layer;

	
	package pac_signal_layers is new ordered_sets (type_signal_layer);
	use pac_signal_layers;
	

	layer_term_start : constant character := '[';
	layer_term_end   : constant character := ']';	
	layer_term_separator : constant character := ',';
	layer_term_range : constant character := '-';
	
	-- Returns a string like "[1,3,5-9]"
	function to_string (layers : in pac_signal_layers.set) return string;


		
	-- Converts a string like [1,3,5-9] to a set 
	-- of signal layers.
	function to_layers (layers : in string) 
		return pac_signal_layers.set;

	
	-- Converts a given single signal layer to a set
	-- that contains just this single layer:
	function to_layers (
		layer	: in type_signal_layer)
		return pac_signal_layers.set;



	-- Converts a line like "layers 1 4 17" or "layers [1,3,4-9]" to 
	-- a set of signal layers.
	-- Issues warning if a layer number occurs more than once.
	-- If layer check requested, issues warning if a layer id is greater than the 
	-- deepest layer used (given in argument check_layer).
	function to_layers (
		line : in type_fields_of_line)
		return pac_signal_layers.set;	

	
	
	-- Mirrors the given layers based on the deepest layer used. The deepest layer is the bottom layer.
	-- Example: signal_layers is a set: 1, 2, 4. The bottom layer id is 4 (an 4-layer board).
	-- The result is: 4, 3, 1. 
	-- The general computation is: 1 + deepest_layer - given_layer = mirrored_layer
	procedure mirror_signal_layers (
		signal_layers	: in out pac_signal_layers.set;
		deepest_layer	: in type_signal_layer);

	
end et_pcb_signal_layers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
