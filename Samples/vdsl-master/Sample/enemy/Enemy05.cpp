/**
 * Domain Specific Visual Programming Language
 * Copyright (c) 2013 NHTV UNIVERSITY OF APPLIED SCIENCES
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Author: Lawrence Kok
 *
 */

#include "../core/object.h"

//enemy 4
Enemy4::Enemy4()
{
	Frame = RandInt(0,5);
	mType = UpperLeft;
}

void Enemy4::Update()
{
		if( tick % 21 == 20 )
		{

			if( IsExploding )
			{
				ExplosionFrame = (ExplosionFrame + 1) % 8;
				return;
			}

			if( ShieldLifetime > 0 )
				ShieldLifetime--;
		}	
	

		if( tick % 7 == 6 )
		{
			if( !player )
			{
			}
			else if( position.x > player->position.x )
			{

				double radiant = atan2( (float)(player->position.y-position.y-10), (float)(player->position.x-position.x+50) );
				position.x += (float)cos(radiant);
				position.y += (float)sin(radiant);
				mType =UpperLeft ;
			}
			else
			{
				double radiant = atan2( (float)(player->position.y-position.y-10), (float)(player->position.x-position.x-50) );
				position.x += (float)cos(radiant);
				position.y += (float)sin(radiant);
				mType = UpperRight;
			}
		
			if( player )
			{
				unsigned short angle = (unsigned short)(atan2( (float)(player->position.y-position.y), (float)(player->position.x-position.x) )  * 32768 / pi);
				switch( mType )
				{
					case UpperRight :
						if( angle > 64000 || angle < 18000)
							Frame = 3;
						else if( angle < 53000 )
							Frame = 1;
						else 
							Frame = 2;
						break;
					case UpperLeft:
						if( angle > 39000)
							Frame = 1;
						else if( angle < 34000 )
							Frame = 3;
						else 
							Frame = 2;
						break;
				}

				if( bulletLifetime > 0 )
					bulletLifetime--;
				else 
				{

					bool canFire = false;
					switch( mType )
					{
						case UpperRight:
							canFire = angle < 65355 && angle > 50000;
							break;
						case UpperLeft:
							canFire = angle < 50000 && angle > 33000;
							break;
					}

					
					if( canFire && RandInt(0,999) > 960 )
					{
						Shoot();
					}
				}
			}





			if( position.x < -30)
				this->IsAlive = false;
		}
		
}



bool Enemy4::IsColliding(Bullet* bullet)
{
	int vx = 0;
	switch(bullet->type)
	{
		case Bullet1:
			vx = 3;
			break;
		case Bullet2:
			vx = 4;
			break;
		case Bullet3:
			vx = 20;
			break;
		case Bullet4:
			vx = 20;
			break;
		case Bullet5:
			vx = 30;
			break;
		case Bullet6:
			vx = 30;
			break;
	}

	return  (bullet->position.x > this->position.x ||  bullet->position.x + vx > this->position.x ) && (bullet->position.x + vx < this->position.x + 40 || bullet->position.x < this->position.x + 40) &&
				bullet->position.y > this->position.y - 50 && bullet->position.y + 17 < this->position.y + 90;
}


void Enemy4::Shoot()
{
	bulletLifetime = 30;

	EnemyBullet2* bullet = new EnemyBullet2();
	bullet->position = position;
	bullet->position.x += 20;
	bullet->position.y -= 10;
	bullet->vx = player->position.x - bullet->position.x;
	bullet->vy = player->position.y - bullet->position.y;
	double d = Distance2D(bullet->vx, bullet->vy);
	bullet->vx /= (float)d;
	bullet->vy /= (float)d;

	v.push_back(bullet);
}
void Enemy4::Draw(Surface *surface)
{
    if( IsExploding )
	{
		switch( ExplosionFrame )
		{
			case 0:
				PlayerGraphics1->AddAreaTo(265,289,283,330, surface,(int)position.x ,(int)position.y);
				break;
			case 1:
				PlayerGraphics1->AddAreaTo(246,289,264,330, surface,(int)position.x ,(int)position.y);
				break;
			case 2:
				PlayerGraphics1->AddAreaTo(214,289,245,330, surface,(int)position.x ,(int)position.y);
				break;
			case 3:
				PlayerGraphics1->AddAreaTo(179,289,213,330, surface,(int)position.x ,(int)position.y);
				break;
			case 4:
				PlayerGraphics1->AddAreaTo(142,289,178,330, surface,(int)position.x ,(int)position.y);
				break;
			case 5:
				PlayerGraphics1->AddAreaTo(106,289,142,330, surface,(int)position.x ,(int)position.y);
				break;
			case 6:
				PlayerGraphics1->AddAreaTo(68,289,105,330, surface,(int)position.x ,(int)position.y);
				break;
			default:
				this->IsAlive = false;
				//score += 35;
				break;
		}
		return;
	}

	switch( mType )
	{
		case UpperRight :
			switch( Frame )
			{
				case 3:
					EnemyGraphics5->PartialCopyAreaTo(165,0,197,30, surface,(int)position.x ,(int)position.y);
					break;
				case 2:
					EnemyGraphics5->PartialCopyAreaTo(132,0,165,30, surface,(int)position.x ,(int)position.y);
					break;
				case 1:
					EnemyGraphics5->PartialCopyAreaTo(99,0,132,30, surface,(int)position.x ,(int)position.y);
					break;
			}

			break;
		case UpperLeft:
			switch( Frame )
			{
				case 1:
					EnemyGraphics5->PartialCopyAreaTo(66,0,99,30, surface,(int)position.x ,(int)position.y);
					break;
				case 2:
					EnemyGraphics5->PartialCopyAreaTo(33,0,66,30, surface,(int)position.x ,(int)position.y);
					break;
				case 3:
					EnemyGraphics5->PartialCopyAreaTo(0,0,33,30, surface,(int)position.x ,(int)position.y);
					break;
			}
			break;
	}

}
