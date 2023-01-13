------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   DEVICE PLACEHOLDERS IN PACKAGES                        --
--                                                                          --
--                              B o d y                                     --
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



package body et_device_placeholders.packages is


	procedure mirror_placeholders (
		placeholders	: in out pac_placeholders.list;
		axis			: in type_axis_2d := Y)
	is
		result : pac_placeholders.list;

		procedure query_placeholder (c : in pac_placeholders.cursor) is
			ph : type_placeholder := element (c);
		begin
			-- CS mirror_text (ph, axis);
			result.append (ph);
		end query_placeholder;
		
	begin
		placeholders.iterate (query_placeholder'access);
		placeholders := result;
	end mirror_placeholders;
	
	

	procedure rotate_placeholders (
		placeholders	: in out pac_placeholders.list;
		angle			: in type_rotation)
	is
		result : pac_placeholders.list;

		procedure query_placeholder (c : in pac_placeholders.cursor) is
			ph : type_placeholder := element (c);
		begin
			-- CS rotate_text (ph, angle);
			result.append (ph);
		end query_placeholder;

	begin
		placeholders.iterate (query_placeholder'access);
		placeholders := result;
	end rotate_placeholders;



	procedure move_placeholders (
		placeholders	: in out pac_placeholders.list;
		offset			: in type_distance_relative)
	is
		result : pac_placeholders.list;

		procedure query_placeholder (c : in pac_placeholders.cursor) is
			ph : type_placeholder := element (c);
		begin
			-- CS move_text (ph, axis);
			result.append (ph);
		end query_placeholder;

	begin
		placeholders.iterate (query_placeholder'access);
		placeholders := result;
	end move_placeholders;

	


	procedure placeholder_properties (
		face			: in type_face;
		cursor			: in pac_placeholders.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_placeholders;
		placeholder : type_placeholder renames element (cursor);
	begin
		log (text => "placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), level => log_threshold);
		
		log_indentation_up;
		log (text => text_properties (type_text (placeholder)), level => log_threshold + 1);
		log_indentation_down;
	end placeholder_properties;

	

	function to_string (layer : in type_placeholder_layer) return string is begin
		return to_lower (type_placeholder_layer'image (layer));
	end;

	function to_layer (layer : in string) return type_placeholder_layer is begin
		return type_placeholder_layer'value (layer);
	end;



	
	
end et_device_placeholders.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
