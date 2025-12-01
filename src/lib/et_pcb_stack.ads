------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              PCB STACK                                   --
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


with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.vectors;
with ada.containers.ordered_sets;


with et_logging;				use et_logging;
with et_board_geometry;			use et_board_geometry;



package et_pcb_stack is
	

	
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

	package type_signal_layers is new ordered_sets (type_signal_layer);
	-- CS rename to pac_signal_layers

	layer_term_start : constant character := '[';
	layer_term_end   : constant character := ']';	
	layer_term_separator : constant character := ',';
	layer_term_range : constant character := '-';
	
	-- Returns a string like "[1,3,5-9]"
	function to_string (layers : in type_signal_layers.set) return string;


	-- Returns true if the given two layer stacks
	-- are equally:
	function layer_stacks_equally (
		right, left : in type_signal_layers.set)
		return boolean;
	
	
	-- Returns true if the given layer stack contains
	-- the given signal layer.
	-- If exclusively is true, then the return is true
	-- if the layer stack contains only the given signal layer:
	function layer_stack_contains (
		stack		: type_signal_layers.set;
		layer		: type_signal_layer;
		exclusively	: in boolean := false)
		return boolean;
	
		
	-- Converts a string like [1,3,5-9] to a set 
	-- of signal layers.
	function to_layers (layers : in string) 
		return type_signal_layers.set;

	
	-- Converts a given single signal layer to a set
	-- that contains just this single layer:
	function to_layers (
		layer	: in type_signal_layer)
		return type_signal_layers.set;

	
	--use pac_geometry_brd;
-- 	subtype type_prepreg_thickness is type_distance_positive range 0.05 .. 0.5; -- CS reasonable ?
-- 	subtype type_core_thickness is type_distance_positive range 0.1 .. 5.0;  -- CS reasonable ?

	use pac_geometry_2;
	
	subtype type_dielectric_thickness is type_distance_positive range 0.01 .. 5.0; -- CS reasonable ?
	dielectric_thickness_default : constant type_dielectric_thickness := 1.5;

	subtype type_conductor_thickness is type_distance_positive range 0.01 .. 0.2;  -- CS reasonable ?
	conductor_thickness_outer_default : constant type_conductor_thickness := 0.035;
	conductor_thickness_inner_default : constant type_conductor_thickness := 0.018;	

	
	type type_conductor is record
		thickness	: type_conductor_thickness := conductor_thickness_outer_default;
		-- CS material specific values
	end record;

	
	type type_dielectric is record
		thickness	: type_dielectric_thickness := dielectric_thickness_default;
		-- CS material specific values
	end record;

	
	-- A layer is a compound of a conductor and a dielectric:
	type type_layer is record
		conductor	: type_conductor;
		dielectric	: type_dielectric;
	end record;

	
	-- The layers are collected in vectors:
	package package_layers is new vectors (
		index_type		=> type_signal_layer,
		element_type	=> type_layer);

	
	-- The final layer stack always has at least the top layer (index 1) 
	-- and the bottom layer. The bottom layer does not have a dielectric.
	type type_stack is record
		layers	: package_layers.vector;
		bottom	: type_conductor;
	end record;

	
	-- Returns the index of the deepest conductor layer:
	function get_deepest_layer (
		stack : in type_stack) 
		return type_signal_layer;

	

	-- If conductor layers must be checked against the deepest layer of the pcb, use
	-- this type:
	type type_do_layer_check is (YES, NO);
	type type_layer_check (check : type_do_layer_check) is record
		case check is
			when NO => null;
			when YES => deepest_layer : type_signal_layer := type_signal_layer'first;
		end case;
	end record;

	
	-- If layer check required, this function returns true if the given layer id
	-- is less or equal the deepest layer used (given by argument check_layers).
	-- Returns false otherwise. 
	-- If no layer check requested, returns true.		
	function signal_layer_valid (
		signal_layer 	: in et_pcb_stack.type_signal_layer;
		check_layers	: in et_pcb_stack.type_layer_check)
		return boolean;

	
	-- Mirrors the given layers based on the deepest layer used. The deepest layer is the bottom layer.
	-- Example: signal_layers is a set: 1, 2, 4. The bottom layer id is 4 (an 4-layer board).
	-- The result is: 4, 3, 1. 
	-- The general computation is: 1 + deepest_layer - given_layer = mirrored_layer
	procedure mirror ( -- CS rename
		signal_layers	: in out type_signal_layers.set;
		deepest_layer	: in type_signal_layer);

	
end et_pcb_stack;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
