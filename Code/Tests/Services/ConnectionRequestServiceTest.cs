using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.Players;
using DDDNetCore.Domain.ConnectionRequests;
using DDDNetCore.Domain.ConnectionRequests.DTOS;
using Moq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Shared;
using System.Linq;

namespace DDDNetCore.Tests.Services
{
    public class ConnectionRequestServiceTest
    {
       [Fact]
        public async Task ApproveRequest_ReturnsApprovedRequestDto()
        {
            // Arrange
            ApproveRequestDto dto = new ApproveRequestDto("request_pending", "middle_target_message2");
            ConnectionRequestId cId = new ConnectionRequestId("12312322-4444-5555-6666-777888999000");
            IntroductionRequest obj = new IntroductionRequest("12312322-4444-5555-6666-777888999000", 
            "12312322-4444-5555-6666-777888999001",
            "middle_target_message", "introduction_pending", "12312322-4444-5555-6666-777888999003",
            "middle_target_message", "middle_target_message", 1, new List<string>{"tag1"});


            var mockRepo = new Mock<IIntroductionRequestRepository>();
            mockRepo.Setup(repo => repo.GetByIdAsync(cId))
                .ReturnsAsync(obj).Verifiable();

            var mockUnity = new Mock<IUnitOfWork>();
            mockUnity.Setup(u => u.CommitAsync());

            var service = new ConnectionRequestService(mockUnity.Object, null, mockRepo.Object, null, null);

            // Act
            var result = await service.ApproveRequest(cId, dto);

            // Assert            
            var returnValue = Assert.IsType<ApproveRequestDto>(result);
            
            Assert.Equal(dto.MiddleManToTargetMessage, returnValue.MiddleManToTargetMessage);
            Assert.Equal(dto.Status, returnValue.Status);
        } 

        [Fact]
        public async Task GetMiddleManRequests_ReturnsRequestWhereThePlayerIsTheMiddleMan()
        {
            // Arrange
            IntroductionRequestDto dto = new IntroductionRequestDto("12312322-4444-5555-6666-777888999000", 
            "12312322-4444-5555-6666-777888999001", 
            "12312322-4444-5555-6666-777888999002",
            "12312322-4444-5555-6666-777888999003", "middle_target_message", "middle_target_message",
            "middle_target_message", "introduction_pending", 1, new List<string>{"tag1"});

            IntroductionRequest obj = new IntroductionRequest("12312322-4444-5555-6666-777888999001", 
            "12312322-4444-5555-6666-777888999003",
            "middle_target_message", "introduction_pending", "12312322-4444-5555-6666-777888999002",
            "middle_target_message", "middle_target_message", 1, new List<string>{"tag1"});

            Player player = new Player("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            List<IntroductionRequest> lst = new List<IntroductionRequest>{obj};


            var mockRepo = new Mock<IIntroductionRequestRepository>();
            mockRepo.Setup(repo => repo.GetMiddleManRequests(player.Id))
                .ReturnsAsync(lst).Verifiable();

            var mockRepoPl = new Mock<IPlayerRepository>();
            mockRepoPl.Setup(repo => repo.GetByEmailAsync(player.Email.address))
                .ReturnsAsync(player);

            var mockUnity = new Mock<IUnitOfWork>();
            mockUnity.Setup(u => u.CommitAsync());

            var service = new ConnectionRequestService(mockUnity.Object, null, mockRepo.Object, null, mockRepoPl.Object);

            // Act
            var result = await service.GetMiddleManRequests(player.Email.address);

            // Assert            
            var returnValue = Assert.IsType<List<IntroductionRequestDto>>(result);
            
            Assert.Equal(dto.CurrentStatus, returnValue.First().CurrentStatus);
            Assert.Equal(dto.MiddleMan, returnValue.First().MiddleMan);
            Assert.Equal(dto.MiddleManToTargetMessage, returnValue.First().MiddleManToTargetMessage);
            Assert.Equal(dto.Player, returnValue.First().Player);
            Assert.Equal(dto.Target, returnValue.First().Target);
            Assert.Equal(dto.PlayerToTargetMessage, returnValue.First().PlayerToTargetMessage);
            Assert.Equal(dto.PlayerToMiddleManMessage, returnValue.First().PlayerToMiddleManMessage);
            Assert.Equal(dto.Strength, returnValue.First().Strength);
            Assert.Equal(dto.Tags, returnValue.First().Tags);
        }          
    }
}