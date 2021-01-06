------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                               VIAS                                       --
--                                                                          --
--                              B o d y                                     --
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

with ada.strings;					use ada.strings;

with et_exceptions;					use et_exceptions;

package body et_vias is
	

	function to_micro_vias_allowed (allowed : in string) return type_micro_vias_allowed is begin
		return type_micro_vias_allowed'value (allowed);
	end to_micro_vias_allowed;
	
	function to_string (allowed : in type_micro_vias_allowed) return string is begin
		return " micro vias allowed " & type_micro_vias_allowed'image (allowed);
	end to_string;


	function to_string (category : in type_via_category) return string is begin
		return to_lower (type_via_category'image (category));
	end to_string;

	function to_via_category (category : in string) return type_via_category is begin
		return type_via_category'value (category);
	end to_via_category;

	
	function to_buried_layers (
		upper, lower	: in string; -- 2, 6
		bottom			: in type_signal_layer) -- 16
		return type_buried_layers
	is
		u, l : type_signal_layer;
		layers : type_buried_layers; -- to be returned

		text : constant string := " invalid. Must be an inner signal layer !";
	begin
		u := to_signal_layer (upper);
		l := to_signal_layer (lower);

		-- Validate upper layer:
		if u > type_signal_layer'first and u < bottom then
			layers.upper := u;
		else
			raise semantic_error_1 with
				"ERROR: Layer " & upper & text;
		end if;

		-- Validate lower layer:
		if u < l then
			if l < bottom then
				layers.lower := l;
			else
				raise semantic_error_1 with
					"ERROR: Layer " & lower & text;
			end if;
		else
			raise semantic_error_1 with
				"ERROR: The lower layer must be deeper than " & upper & " !";
		end if;
		
		return layers;
	end to_buried_layers;

	function to_string (layers : in type_buried_layers) return string is
	begin
		return to_string (layers.upper) & space & to_string (layers.lower);
	end to_string;


end et_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
