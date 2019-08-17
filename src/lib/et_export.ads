------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               EXPORT                                     --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with ada.text_io;				use ada.text_io;
with ada.directories;			use ada.directories;

with et_general;
with et_string_processing;		--use et_string_processing;

package et_export is


	-- CAD FORMATS
	type type_cad_format is (et_v1, kicad_v4);
	
	-- If no format specified via cmd line, a default applies so that the operator can be 
	-- notified about missing cad format.
	cad_format : type_cad_format := type_cad_format'first; 

	directory_export		: constant string := "export";
	directory_cam			: constant string := "CAM";
	-- directory_cad			: constant string := "CAD";	
	directory_statistics	: constant string := "statistics";
	
	procedure create_project_directory (
	-- Creates given project directory in work_directory of ET.
		project			: in string;
		log_threshold	: in et_string_processing.type_log_level);

end et_export;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
