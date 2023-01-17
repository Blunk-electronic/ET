------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   DEVICE PLACEHOLDERS IN SYMBOLS                         --
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


package body et_device_placeholders.symbols is

	procedure write_placeholder_properties (
		placeholder		: in type_text_placeholder;
		log_threshold	: in type_log_level) 
	is begin
		-- meaning
		log (text => to_string (placeholder.meaning), level => log_threshold);
		log_indentation_up;
		
		-- position
		log (text => to_string (placeholder.position), level => log_threshold);

		-- size
		log (text => to_string (placeholder.size), level => log_threshold);

		-- rotation
		log (text => to_string (placeholder.rotation), level => log_threshold); 

		-- visible
		--log (text => "visible "
		--	& to_lower (et_libraries.type_text_visible'image (placeholder.visible)), level => log_threshold);

		-- alignment
		log (text => et_text.to_string (placeholder.alignment),
			level => log_threshold);

		log_indentation_down;
	end write_placeholder_properties;

	
end et_device_placeholders.symbols;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
