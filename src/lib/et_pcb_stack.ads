------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              PCB STACK                                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

-- with ada.containers.doubly_linked_lists;
with ada.containers.vectors;
-- with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_string_processing;
with et_pcb_coordinates;		use et_pcb_coordinates;

package et_pcb_stack is

	keyword_conductor	: constant string := "conductor";
	keyword_dielectric	: constant string := "dielectric";	
	keyword_bottom		: constant string := "bottom";	
	keyword_layer		: constant string := "layer";	
	keyword_layers		: constant string := "layers";
	
	signal_layer_top : constant positive := 1; -- CS rename signal to conductor
	signal_layer_bot : constant positive := 100;
	type type_signal_layer is range signal_layer_top .. signal_layer_bot;

	function to_string (layer : in type_signal_layer) return string;
	function to_signal_layer (layer : in string) return type_signal_layer;

	package type_signal_layers is new ordered_sets (type_signal_layer);

	layer_term_start : constant character := '[';
	layer_term_end   : constant character := ']';	
	layer_term_separator : constant character := ',';
	layer_term_range : constant character := '-';
	
	function to_string (layers : in type_signal_layers.set) return string;
	-- Returns a string like "[1,3,5-9]"

	function to_layers (layers : in string) return type_signal_layers.set;
	-- converts a string like [1,3,5-9] to a set of signal layers.
	
	use geometry;
-- 	subtype type_prepreg_thickness is type_distance_positive range 0.05 .. 0.5; -- CS reasonable ?
-- 	subtype type_core_thickness is type_distance_positive range 0.1 .. 5.0;  -- CS reasonable ?

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

	-- Returns the index of the greatest conductor layer:
	function greatest_layer (stack : in type_stack) return type_signal_layer;
		
	
end et_pcb_stack;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
