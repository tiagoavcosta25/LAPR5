using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.Players;
using DDDSample1.Controllers;
using Moq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Controllers
{
    public class PlayerControllerTest
    {

        [Fact]
        public async Task Create_ReturnsAPlayerDto_WithPlayerData()
        {
            // Arrange
            CreatingPlayerDto dto = new CreatingPlayerDto("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            /*Player obj = new Player(new PlayerName("john"), new PlayerName("test@email.com"), new PlayerPassword("123"), new PlayerPhoneNumber("987654321"),
                 new PlayerDateOfBirth(2001, 1, 17), new PlayerEmotionalStatus(OOC.joyful), new PlayerFacebook("www.facebook.com/john-doe"), 
                new PlayerLinkedIn("www.linkedin.com/john-doe"), new List<Tag>{new Tag("tag")});*/
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.AddAsync(dto))
                .ReturnsAsync(obj).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.Create(dto);

            // Assert
            var actionResult = Assert.IsType<CreatedAtActionResult>(result.Result);
            
            var returnValue = Assert.IsType<PlayerDto>(actionResult.Value);
            mockServ.Verify();
            
            Assert.Equal(obj.Email, returnValue.Email);
        }

        [Fact]
        public async Task Create_ReturnsBadRequestResult_WhenEmptyEmail()
        {
            // Arrange
            CreatingPlayerDto dto = new CreatingPlayerDto("john", "", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.AddAsync(dto))
                .ThrowsAsync(new BusinessRuleValidationException(""));
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.Create(dto);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.IsType<BadRequestObjectResult>(actionResult);
        }
    }
}