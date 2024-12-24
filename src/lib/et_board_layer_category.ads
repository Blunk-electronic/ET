------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD LAYER CATEGORY                               --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

with ada.containers;					use ada.containers;
with ada.containers.vectors;



package et_board_layer_category is


	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:	
	layer_category_prefix : constant string := "LAYER_CAT_";

	
	type type_layer_category is (
									
		-- CONDUCTOR LAYERS.
		-- These layers are numbered:
		LAYER_CAT_CONDUCTOR,
		
		-- NON CONDUCTOR LAYERS.
		-- These layers are paired. Means there is a TOP and a BOTTOM:
		LAYER_CAT_SILKSCREEN,
		LAYER_CAT_ASSY,
		LAYER_CAT_STOP,
		
		LAYER_CAT_KEEPOUT,
		LAYER_CAT_STENCIL,

		
		-- NOTE: Restrict layers do not contain any conducting
		-- objects. They are irrelevant for manufacturing.
		-- Since they are of mere supportive nature for routing
		-- we regarded them as conductor layers.
		-- These layers are numbered:
		LAYER_CAT_ROUTE_RESTRICT,
		LAYER_CAT_VIA_RESTRICT);

	
	
	function to_layer_category (
		cat : in string) 
		return type_layer_category;

	
	function to_string (
		cat : in type_layer_category) 
		return string;



	-- For collecting layer categories we use a so called vector:
	package pac_affected_layer_categories is new vectors (
		index_type		=> natural,
		element_type	=> type_layer_category);

	
	
end et_board_layer_category;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
