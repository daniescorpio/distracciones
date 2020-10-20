with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts       
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    -----------------------------------------------------------------------
    ------------- constants 
    -----------------------------------------------------------------------
    
    LEER_POSICION_CABEZA_PRIORITY 	  : Integer   := 1;
    LEER_DISTANCIA_PRIORITY 	  	  : Integer   := 2;
    MOSTRAR_INFO_DISPLAY_PRIORITY  	  : Integer   := 3;
    CALCULAR_RIESGOS_PRIORITY	  	  : Integer   := 4;
    LEER_GIRO_VOLANTE_PRIORITY 	  	  : Integer   := 5;
    INTERVALO_LECTURA_POSICION_CABEZA     : Time_Span := Milliseconds (400);
    INTERVALO_LECTURA_DISTANCIA_SEGURIDAD : Time_Span := Milliseconds (300);
    INTERVALO_LECTURA_GIRO_VOLANTE        : Time_Span := Milliseconds (350);
    INTERVALO_DETECCION_RIESGOS		  : Time_Span := Milliseconds (150);
    INTERVALO_REFRESCO_DISPLAY		  : Time_Span := Milliseconds (1000);
    
    
    -----------------------------------------------------------------------
    ------------- custom types 
    -----------------------------------------------------------------------
    
    type Cabeza_Inclinada_Estado_Type is (CABEZA_INCLINADA, CABEZA_NO_INCLINADA);
    type Volantazo_Estado_Type is (ESTADO_VOLANTAZO, ESTADO_NO_VOLANTAZO);
    type Distancia_Estado_Type is (DISTANCIA_PELIGROSA, DISTANCIA_INSEGURA, DISTANCIA_IMPRUDENTE, DISTANCIA_SEGURA);
    
    -----------------------------------------------------------------------
    ------------- declaration of protected objects 
    -----------------------------------------------------------------------

    Protected Sintomas is
      procedure Guardar_Estado_Distraccion (actual_Distraccion : in Cabeza_Inclinada_Estado_Type);
      procedure Guardar_Estado_Volantazo (actual_Volantazo : in Volantazo_Estado_Type);
      procedure Guardar_Estado_Distancia (actual_Distancia : in Distancia_Estado_Type);
      function Obtener_Estado_Distraccion return Cabeza_Inclinada_Estado_Type;
      function Obtener_Estado_Volantazo return Volantazo_Estado_Type;
      function Obtener_Estado_Distancia return Distancia_Estado_Type;
    private
      CABEZA_INCLINADA : Cabeza_Inclinada_Estado_Type;
      VOLANTAZO	       : Volantazo_Estado_Type;
      DISTANCIA: Distancia_Estado_Type;
      
    end Sintomas;
    
    --Protected Medidas is
    --private
    --end Medidas;
    
    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    -- Aqui se declaran las tareas que forman el STR
    
    task LeerPosicionCabeza is 
      pragma priority (LEER_POSICION_CABEZA_PRIORITY);
    end leerPosicionCabeza;
    
    task LeerDistancia is 
      pragma priority (LEER_DISTANCIA_PRIORITY);
    end LeerDistancia;
    
    task MostrarInfoDisplay is
      pragma priority (MOSTRAR_INFO_DISPLAY_PRIORITY);
    end; 
    
    task CalcularRiesgos is
      pragma priority (CALCULAR_RIESGOS_PRIORITY);
    end;
    
    task LeerGiroVolante is
      pragma priority (LEER_GIRO_VOLANTE_PRIORITY);
    end leerGiroVolante;


    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------
    
    -----------------------------------------------------------------------
    ------------- TAREA POSICION CABEZA 
    -----------------------------------------------------------------------
    
    task body LeerPosicionCabeza is 
      Current_H  	 : HeadPosition_Samples_Type;
      Previous_H 	 : HeadPosition_Samples_Type := (0, 0);
      Current_S     	 : Steering_Samples_Type;
      Distraccion 	 : Cabeza_Inclinada_Estado_Type;
      Siguiente_Instante : Time;
      
    begin
    
      Siguiente_instante := Clock + INTERVALO_LECTURA_POSICION_CABEZA;
      Starting_Notice ("Prueba sensor cabeza");
      
      loop
      
        Distraccion := CABEZA_NO_INCLINADA;
      
        Reading_HeadPosition(Current_H);
        Reading_Steering (Current_S);
        
        if (Current_H(x) > 30 AND Previous_H(x) > 30) then 
          Distraccion := CABEZA_INCLINADA;
        end if;
        
        if (Current_H(x) < -30 AND Previous_H(x) < -30) then
          Distraccion := CABEZA_INCLINADA;
        end if;
        
        if (Current_H(y) > 30 AND Previous_H(y) > 30 AND Current_S <= 0) then 
          Distraccion := CABEZA_INCLINADA;
        end if;
        
        if (Current_H(y) < -30 AND Previous_H(y) < -30 AND Current_S >= 0) then
          Distraccion := CABEZA_INCLINADA;
        end if;
        
        Previous_H := Current_H;
        Sintomas.Guardar_Estado_Distraccion(Distraccion);
        Distraccion := CABEZA_NO_INCLINADA;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_POSICION_CABEZA;
        
      end loop;
      
      Finishing_Notice ("Prueba sensor cabeza");
      
    end leerPosicionCabeza;
    
    
    
    -----------------------------------------------------------------------
    ------------- TAREA DISTANCIA DE SEGURIDAD 
    -----------------------------------------------------------------------

     task body LeerDistancia is
       Current_D 	   : Distance_Samples_Type := 0;
       Current_V 	   : Speed_Samples_Type    := 0;
       Distancia_Seguridad : Float		   := 0.0;
       Distancia 	   : Distancia_Estado_Type;
       Siguiente_Instante  : Time;
 
     begin
     
       Siguiente_instante := Clock + INTERVALO_LECTURA_DISTANCIA_SEGURIDAD;
       Starting_Notice ("Prueba sensor distancia");
	
       loop
      
	Distancia := DISTANCIA_SEGURA;

         Reading_Distance (Current_D);      
         Reading_Speed (Current_V);
	 Distancia_Seguridad:= (Float(Current_V) / 10.0) ** 2;
        
         if (Float(Current_D) > Distancia_Seguridad) then
           Distancia := DISTANCIA_INSEGURA;
         elsif(Float(Current_D) > Distancia_Seguridad / 2.0) then
	   Distancia := DISTANCIA_IMPRUDENTE;
	 elsif(Float(Current_D) > Distancia_Seguridad / 3.0 ) then
	   Distancia := DISTANCIA_PELIGROSA;
	 end if;

	Sintomas.Guardar_Estado_Distancia(Distancia);
	Distancia := DISTANCIA_SEGURA;	

	delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_DISTANCIA_SEGURIDAD;
        
        end loop;
      
    	Finishing_Notice ("Prueba sensor distancia");
      
    end leerDistancia;
    
 
    -----------------------------------------------------------------------
    ------------- TAREA MOSTRAR INFO DISPLAY 
    -----------------------------------------------------------------------
        
    task body MostrarInfoDisplay is
      Distraccion 	 : Cabeza_Inclinada_Estado_Type;
      Siguiente_Instante : Time;
      Distancia 	 : Distancia_Estado_Type;
      Volantazo   	 : Volantazo_Estado_Type;
      Current_V : Speed_Samples_Type := 0;

    begin
      
      Siguiente_instante := Clock + INTERVALO_REFRESCO_DISPLAY;
      Starting_Notice ("Prueba display");
      
      loop
        
        Distraccion := Sintomas.Obtener_Estado_Distraccion;
	Distancia:= Sintomas.Obtener_Estado_Distancia;
	Volantazo:= Sintomas.Obtener_Estado_Volantazo;
        

	if (Distraccion /= CABEZA_NO_INCLINADA OR Distancia/=DISTANCIA_SEGURA OR Volantazo/=ESTADO_NO_VOLANTAZO) then
	  Put ("............%");
          Put ("¡¡¡ DISTRACCION DETECTADA !!!");
	  New_line;
	  
	  Reading_Speed (Current_V);
	  Display_Speed (Current_V);
	  New_line;
	  
	  if (Distraccion /= CABEZA_NO_INCLINADA) then
		Put (" Se han detectado movimientos inusuales en su cabeza ");
		New_line;
	  end if;

          if (Distancia /= DISTANCIA_SEGURA) then
		Put (" La distancia respecto al coche de alante es inapropiada");
		New_line;
	  end if;

	  if(Volantazo /= ESTADO_NO_VOLANTAZO) then
		Put(" Se han detectado movimientos inusuales en el volante ");
		New_line;
	  end if;
	
	end if;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_REFRESCO_DISPLAY;
        
      end loop;
      
      Finishing_Notice ("Prueba display");
      
    end;
    
    
    -----------------------------------------------------------------------
    ------------- TAREA DETECCION DE RIESGOS 
    -----------------------------------------------------------------------
        
    task body CalcularRiesgos is
      Distraccion 	 : Cabeza_Inclinada_Estado_Type;
      Volantazo 	 : Volantazo_Estado_Type;
      Current_V 	 : Speed_Samples_Type    := 0;
      Distancia		 : Distancia_Estado_Type;
      Siguiente_Instante : Time;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_DETECCION_RIESGOS;
      Starting_Notice ("Prueba deteccion de riesgos");
      
      loop
      
        Reading_Speed (Current_V);
        Distraccion := Sintomas.Obtener_Estado_Distraccion;
        if (Distraccion = CABEZA_INCLINADA) then
          if (Current_V > 70) then
            Beep(3);
          else 
            Beep(2);
          end if; 
        end if;
        
        Volantazo := Sintomas.Obtener_Estado_Volantazo;
        if (Volantazo = ESTADO_VOLANTAZO) then
          Beep(1);
        end if;
	
	Distancia:=Sintomas.Obtener_Estado_Distancia;
	if (Distancia = DISTANCIA_INSEGURA) then
	  Light(On);
	elsif (Distancia = DISTANCIA_IMPRUDENTE) then
	  Light(On);
	  Beep(4);
	elsif (Distancia = DISTANCIA_PELIGROSA) then
	  Beep(5);
        end if;

        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_DETECCION_RIESGOS;
        
      end loop;
      
      Finishing_Notice ("Prueba deteccion de riesgos");
      
    end;
    
    
    -----------------------------------------------------------------------
    ------------- TAREA GIRO VOLANTE 
    -----------------------------------------------------------------------
    
    task body LeerGiroVolante is
    
      Current_S     	 : Steering_Samples_Type;
      Previous_S    	 : Steering_Samples_Type := 0;
      Current_V 	 : Speed_Samples_Type    := 0;
      Actual_Offset 	 : Steering_Samples_Type;
      Siguiente_Instante : Time;
      Volantazo   	 : Volantazo_Estado_Type;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_LECTURA_GIRO_VOLANTE;
      Starting_Notice ("Prueba sensor giro volante");
      
      loop
        
        Volantazo := ESTADO_NO_VOLANTAZO;
        
        Reading_Steering (Current_S);
        Reading_Speed (Current_V);
        
        Actual_Offset := Current_S - Previous_S;
        
        if ((Actual_Offset > 20 OR Actual_Offset < -20) AND Current_V > 40) then 
          Volantazo := ESTADO_VOLANTAZO;
        end if;
        
        Previous_S := Current_S;
        Sintomas.Guardar_Estado_Volantazo(Volantazo);
        Volantazo := ESTADO_NO_VOLANTAZO;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_GIRO_VOLANTE;
        
      end loop;
      
      Finishing_Notice ("Prueba sensor giro volante");
      
    end leerGiroVolante;
    
    
    -----------------------------------------------------------------------
    ------------- body of protected objects 
    -----------------------------------------------------------------------
    
    protected body Sintomas is
      
      procedure Guardar_Estado_Distraccion(actual_Distraccion: in Cabeza_Inclinada_Estado_Type) is
      begin
        CABEZA_INCLINADA := actual_Distraccion;
      end Guardar_Estado_Distraccion;
      
      procedure Guardar_Estado_Volantazo (actual_Volantazo : in Volantazo_Estado_Type) is 
      begin
        VOLANTAZO := actual_Volantazo;
      end Guardar_Estado_Volantazo;

      procedure Guardar_Estado_Distancia (actual_Distancia : in Distancia_Estado_Type) is 
      begin
        DISTANCIA := actual_Distancia;
      end Guardar_Estado_Distancia;
      
      function Obtener_Estado_Distraccion return Cabeza_Inclinada_Estado_Type is
      begin 
        return CABEZA_INCLINADA;
      end Obtener_Estado_Distraccion;
      
      function Obtener_Estado_Volantazo return Volantazo_Estado_Type is
      begin
        return VOLANTAZO;
      end Obtener_Estado_Volantazo;

      function Obtener_Estado_Distancia return Distancia_Estado_Type is
      begin
        return DISTANCIA;
      end Obtener_Estado_Distancia;
      
    end Sintomas;
    
    
    --protected body Medidas is
    
      --function LeerSensorCabeza return HeadPosition_Samples_Type is
        --h : HeadPosition_Samples_Type;
      --begin
        --Reading_HeadPosition(h);
        --return h;
      --end LeerSensorCabeza;
      
    --end Medidas;
    
   
    ----------------------------------------------------------------------
    ------------- procedure para probar los dispositivos 
    ----------------------------------------------------------------------
    procedure Prueba_Dispositivos; 

    Procedure Prueba_Dispositivos is
        Current_V : Speed_Samples_Type := 0;
        Current_H : HeadPosition_Samples_Type := (+2,-2);
        Current_D : Distance_Samples_Type := 0;
        Current_O : Eyes_Samples_Type := (70,70);
        Current_E : EEG_Samples_Type := (1,1,1,1,1,1,1,1,1,1);
        Current_S : Steering_Samples_Type := 0;
    begin
         --Starting_Notice ("Prueba_Dispositivo");

         --for I in 1..120 loop
         -- Prueba distancia
            --Reading_Distance (Current_D);
            --Display_Distance (Current_D);
            --if (Current_D < 40) then Light (On); 
            --                    else Light (Off); end if;

         -- Prueba velocidad
            --Reading_Speed (Current_V);
            --Display_Speed (Current_V);
            --if (Current_V > 110) then Beep (2); end if;

         -- Prueba volante
            --Reading_Steering (Current_S);
            --Display_Steering (Current_S);
            --if (Current_S > 30) OR (Current_S < -30) then Light (On);
            --                                         else Light (Off); end if;

         -- Prueba Posicion de la cabeza
            --Reading_HeadPosition (Current_H);
            --Display_HeadPosition_Sample (Current_H);
            --if (Current_H(x) > 30) then Beep (4); end if;

         -- Prueba ojos
            --Reading_EyesImage (Current_O);
            --Display_Eyes_Sample (Current_O);

         -- Prueba electroencefalograma
            --Reading_Sensors (Current_E);
            --Display_Electrodes_Sample (Current_E);
   
         --delay until (Clock + To_time_Span(0.1));
         --end loop;

         --Finishing_Notice ("Prueba_Dispositivo");
         null;
    end Prueba_Dispositivos;


begin
   null;
   --Starting_Notice ("Programa Principal");
   --Prueba_Dispositivos;
   --Finishing_Notice ("Programa Principal");
end add;



