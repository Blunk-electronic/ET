------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        DEVICE PLACEHOLDERS                               --
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



package body et_device_placeholders is

	function to_string (
		text_meaning : in type_placeholder_meaning) 
		return string 
	is begin
		return to_lower (type_placeholder_meaning'image (text_meaning));
	end;

	
	function to_meaning (
		text_meaning : in string) 
		return type_placeholder_meaning 
	is begin
		return type_placeholder_meaning'value (text_meaning);
	end;


	
	function to_string (
		mode : in type_anchor_mode)
		return string
	is begin
		return to_lower (type_anchor_mode'image (mode));
	end;

	

	function to_anchor_mode (
		mode : in string)
		return type_anchor_mode
	is begin
		return type_anchor_mode'value (mode);
	end;

	
	
end et_device_placeholders;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
